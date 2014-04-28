
source('gibbs.parallel.R')
source('gibbs.R')

gibbs.vis.stat <- function(dataset, n.save = 1000)
{
  params.grid <- sampling.params.grid()
  
  do.plot <- F
  
  multi.run.save.fn <- paste(
    base.dir, 
    'multi.run-', 
    dataset$digest, 
    '.rdb', 
    sep = '')
  
  if (do.plot || !file.exists(multi.run.save.fn) || T)
  {
    multi.run <- c()
    #for(i in 1:nrow(params.grid))
    for (i in c(100))
    {
      params <- as.list(params.grid[i, ])
      ratio <- params$ratio
      K <- params$K
      repi <- params$rep
      
      lda.save.fn <- gibbs.lda.save.fn(K, n.save, repi, ratio, dataset$digest)
      single.run <- plot.single.run(lda.save.fn, params, do.plot)
      
      multi.run <- rbind(multi.run, 
                         as.data.frame(single.run, row.names = i))
      stop('enough')
    }
    
    save(multi.run, file = multi.run.save.fn)
  } else
    load(multi.run.save.fn)
  
  multi.summary <- summary.multiple.run(multi.run)
  return(multi.summary)
}

library(ggplot2)
library(reshape2)
summary.multiple.run <- function(multi.run)
{
  plot.table <- function(table, title)
  {
    plot.obj <- ggplot(melt(table), aes(x = ratio, y = value, group = K, color = K)) +
      geom_line() +
      geom_point(size = 5) +
      ggtitle(title)
    print(plot.obj)
  }
  perp.summary <- tapply(
    cbind(multi.run[, 'perplexity']), 
    cbind(multi.run[, c('K', 'ratio')]), 
    FUN = mean)
  plot.table(perp.summary, 'Perplexity')
  
  multi.summary <- list()
  multi.summary$perplexity <- perp.summary
  
  if ('bias' %in% colnames(multi.run))
  {
    bias.summary <- tapply(multi.run[, 'bias'], multi.run[, c('K', 'ratio')], FUN = mean)
    plot.table(bias.summary, 'Instance aggreement correlation')
    multi.summary$bias <- bias.summary
  }
  
  return(multi.summary)
}

plot.single.run <- function(res.fn, params, do.plot = T)
{
  load(res.fn)
  vocab <- train.dev$vocab
  
  thin <- 20
  
  single.run <- params
  
  single.run$thin <- thin
  
  cat(res.fn, '\n')
  
  ## plot perplexity ~ iterations
  
  perp.res <- as.vector(perp.res)
  
  if (do.plot)
    plot(thin * (1:length(perp.res)),
         perp.res,
         type = 'l',
         xlab = 'Iters', ylab = 'Perplexity', main = 'Perplexity over iterations')
  
  single.run$perplexity <- perp.res[length(perp.res)]
  
  ## plot bias
  if (exists('bias.res') && length(bias.res) != 0)
  {
    bias.res <- as.vector(bias.res)
    
    if (do.plot)
      plot(thin * (1:length(bias.res)),
           bias.res,
           type = 'l',
           xlab = 'Iters', ylab = 'Correlation', main = 'Sample aggrement correlation')
    single.run$bias <- bias.res[length(bias.res)]
  }
  
  
  ## plot trace
  alpha <- .01
  beta <- .05
  
  top.k.topic <- 2
  
  subsetting.trace <- function(big.trace)
  {
    # keep two topics for each document
    trace.sum <- apply(big.trace, c(1, 2), sum)
    trace.topic <- apply(
      trace.sum,
      2,
      function(x)
        sort(x, decreasing = T, index.return = T)$ix
    )
    trace.topic <- trace.topic[1:top.k.topic, ]
    
    trace.subset <- abind(
      lapply(
        1:ncol(trace.topic),
        function(i)
          big.trace[trace.topic[, i], i, ,drop = F]
      ),
      along = 2
    )
    
    res <- list('trace.subset' = trace.subset, 'trace.topic' = trace.topic - 1L)
  }
  
  trace.mean <- function(big.trace)
  {
    res <- apply(big.trace, c(1, 2), FUN = mean)
    dim(res) <- dim(big.trace)[1:2]
    return(res)
  }
  
  library('gibbsLda')
  gen.theta.kd.trace <- function()
  {
    Ndw <- table_1d_fast(train.dev$train[, 'doc'], train.dev$n.docs)
    
    doc.id <- train.dev$doc.trace.list
    doc.len <- Ndw[doc.id]
    
    theta.dk.trace <- apply(doc.trace + alpha, c(2, 3), '/', doc.len + n.topics * alpha)
    theta.kd.trace <- aperm(theta.dk.trace, c(2, 1, 3))
    
    theta.kd.mean <- trace.mean(theta.kd.trace)
    res <- subsetting.trace(theta.kd.trace)
    
    res$doc.id <- train.dev$doc.trace.list
    res$trace.mean <- theta.kd.mean
    
    return(res)
  }
  
  gen.phi.kw.trace <- function()
  {
    phi.trace <- apply(word.trace + beta, 1, '/', nk.trace + train.dev$n.words * beta)
    
    dim(phi.trace) <- c(n.topics, dim(word.trace)[3], train.dev$word.trace.n)
    phi.kw.trace <- aperm(phi.trace, c(1, 3, 2))
    
    phi.kw.mean <- trace.mean(phi.kw.trace)
    
    res <- subsetting.trace(phi.kw.trace)
    res$word.id <- train.dev$word.trace.list
    res$trace.mean <- phi.kw.mean
    
    return(res)
  }
  
  library(coda)
  trace.to.mcmc <- function(trace)
  {
    if (is.null(trace$word.id))
    {
      name <- 'doc'
      name.list <- trace$doc.id
    }
    else
    {
      name <- 'word'
      name.list <- trace$word.id
      
      if (!is.null(vocab))
        name.list <- vocab[name.list + 1]
    }
    
    topic.list <- trace$trace.topic
    trace.samples <- trace$trace.subset
    
    trace.dim <- dim(trace.samples)
    dim(trace.samples) <- c(trace.dim[1] * trace.dim[2], trace.dim[3])
    
    rownames(trace.samples) <- paste(name, rep(name.list, each = nrow(topic.list)),
                                     'topic', as.vector(topic.list), sep = "-")
    
    return(as.mcmc(t(trace.samples)))
  }
  
  trace.mcmc.plot <- function(samples.mcmc)
  {
    oldpar <- par(mfrow = c(top.k.topic, 2))
    plot(samples.mcmc, smooth = T, auto.layout = F, ask = F)
    par(oldpar)
  }
  
  plot.trace <- function(trace.gen.func)
  {
    trace <- trace.gen.func()
    if (do.plot)
    {
      trace.mcmc <- trace.to.mcmc(trace)
      trace.mcmc.plot(trace.mcmc)
    }
    return(trace$trace.mean)
  }
  
  fetch.examples <- function()
  {
    Ndk <- table_2d_fast(train.dev$train[, 'doc'], Z, train.dev$n.docs, n.topics)
    Nwk <- table_2d_fast(train.dev$train[, 'word'], Z, train.dev$n.words, n.topics)
    Nk <- table_1d_fast(Z, n.topics)
    Ndw <- table_1d_fast(train.dev$train[, 'doc'], train.dev$n.docs)
    
    theta_dk <- apply(Ndk + alpha, 2, '/', Ndw + n.topics * alpha)
    phi_kw <- apply(Nwk + beta, 1, '/', Nk + train.dev$n.words * beta)
    
    top.topic.n <- 10
    top.topic <- sort(Nk, decreasing = T, index.return = T)$ix
    top.topic <- top.topic[1:top.topic.n]
    
    n.top.w <- 10
    phi.wk.top.w <- apply(phi_kw, 1,
                          function(x)
                            sort(x, decreasing = T, index.return = T)$ix)[1:n.top.w, top.topic]
    
    phi.wk.top.w <- vocab[phi.wk.top.w, ]
    
    dim(phi.wk.top.w) <- c(n.top.w, top.topic.n)
    
    write.table(t(phi.wk.top.w), file = 'ap/top.w', 
                quote = F, row.names = F, col.names = F)
    print(phi.wk.top.w)
    
    n.top.docs <- 10
    
    theta.dk.top.doc <- apply(
      theta_dk, 2,
      function(x)
        sort(x, decreasing = T, index.return = T)$ix)[1:n.top.docs, top.topic]
    
    print(theta.dk.top.doc)
    write.table(t(theta.dk.top.doc), file = 'ap/top.doc', 
                quote = F, row.names = F, col.names = F)
  }
  
  if (!is.null(vocab))
    fetch.examples()
  
  #mean.theta.kd <<- plot.trace(gen.theta.kd.trace)
  #mean.phi.kw <<- plot.trace(gen.phi.kw.trace)
  
  return(single.run)
}


#res.fn <- 'data-cluster/bst512-ap/bst512/perp.res-50-1000-5-0.2.rdb'
#res.fn <- 'data-cluster/bst512-sim/bst512-sim/perp.res-general-10-1000-1-0.2.rdb'

#plot.single.run(res.fn, list())
