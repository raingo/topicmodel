
source('gibbs.parallel.R')
source('gibbs.R')

gibbs.vis.stat <- function(dataset)
{
  params.grid <- sampling.params.grid()
  for(i in 1:nrow(params.grid))
  {
    ratio <- params.grid[i, 'ratio']
    K <- params.grid[i, 'K']
    repi <- params.grid[i, 'rep']
    
    lda.save.fn <- gibbs.lda.save.fn(K, n.save, repi, train.ratio, data.name)
  }
}

res.fn <- 'data-cluster/bst512-ap/bst512/perp.res-50-1000-5-0.2.rdb'
vocab <- as.matrix(read.table('vocab.txt'))

#res.fn <- 'data-cluster/bst512-sim/bst512-sim/perp.res-general-10-1000-1-0.2.rdb'
load(res.fn)

thin <- 20

## plot perplexity ~ iterations
perp.res <- as.vector(perp.res)
plot(thin * (1:length(perp.res)), 
     perp.res, 
     type = 'l', 
     xlab = 'Iters', ylab = 'Perplexity', main = 'Perlexity over iterations')

## plot trace

library('gibbsLda')

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

gen.theta.dk.trace <- function()
{
  Ndw <- table_1d_fast(train.dev$train[, 'doc'], train.dev$n.docs)
  
  doc.id <- train.dev$doc.trace.list
  doc.len <- Ndw[doc.id]
  
  theta.dk.trace <- apply(doc.trace + alpha, c(2, 3), '/', doc.len + n.topics * alpha)
  theta.kd.trace <- aperm(theta.dk.trace, c(2, 1, 3))
  
  res <- subsetting.trace(theta.kd.trace)
  
  res$doc.id <- train.dev$doc.trace.list
  
  return(res)
}

gen.phi.kw.trace <- function()
{
  phi.trace <- apply(word.trace + beta, 1, '/', nk.trace + train.dev$n.words * beta)

  dim(phi.trace) <- c(n.topics, dim(word.trace)[3], train.dev$word.trace.n)
  phi.kw.trace <- aperm(phi.trace, c(1, 3, 2))
  
  res <- subsetting.trace(phi.kw.trace)
  res$word.id <- train.dev$word.trace.list
  
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
  trace.mcmc <<- trace.to.mcmc(trace)
  trace.mcmc.plot(trace.mcmc) 
}

#plot.trace(gen.theta.dk.trace)
#plot.trace(gen.phi.kw.trace)