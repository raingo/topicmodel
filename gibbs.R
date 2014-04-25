#setwd('/home/raingo/workplace/BST512/project/code')
#setwd('/u/yli/workplace/bst512/')
library(doParallel)
library(abind)

doc.pairs <- as.matrix(read.table('ap-pairs.dat'))
colnames(doc.pairs) <- c('doc', 'word', 'cnt')
storage.mode(doc.pairs) <- 'integer'

vocab <- read.table('vocab.txt')

data.split <- function(triple.let, train.ratio = .8)
{
  train.dev <- runif(nrow(triple.let)) < train.ratio
  
  n.words <- max(triple.let[, 2]) + 1L
  n.docs <- max(triple.let[, 1]) + 1L
  
  word.trace.n <- 5L
  word.trace.list <- as.integer(sample.int(n.words, size = word.trace.n, replace = F)) - 1L
  
  doc.trace.n <- 5L
  doc.trace.list <- as.integer(sample.int(n.docs, size = doc.trace.n, replace = F)) - 1L
  
  train <- triple.let[train.dev, ]
  dev <- triple.let[!train.dev, ]
  
  res <- list('train' = train, 
              'dev' = dev, 
              'n.words' = n.words, 
              'n.docs' = n.docs, 
              'train.ratio' = train.ratio,
              'word.trace.n' = word.trace.n,
              'word.trace.list' = word.trace.list,
              'doc.trace.n' = doc.trace.n,
              'doc.trace.list' = doc.trace.list)
  return(res)
}

gibbs.lda <- function(train.dev, n.topics, n.save = 100, repi = 0)
{
  #save.fn <- paste('data/', 'perp.res-', n.topics, '-', n.save, '-', repi, '-', train.dev$train.ratio, '.rdb', sep='')
  save.fn <- paste('/scratch/yli-data/bst512/', 'perp.res-', n.topics, '-', n.save, '-', repi, '-', train.dev$train.ratio, '.rdb', sep='')
  if(file.exists(save.fn))
  {
    cat('loading existing data')
    load(save.fn)
  }
  else{
    seed0 <- 12345
    set.seed(seed0)
    
    Z <- as.integer(sample.int(n.topics, size = nrow(train.dev$train), replace = T)) - 1L
    
    Ndk <- table_2d_fast(train.dev$train[, 'doc'], Z, train.dev$n.docs, n.topics)
    Nwk <- table_2d_fast(train.dev$train[, 'word'], Z, train.dev$n.words, n.topics)
    Nk <- table_1d_fast(Z, n.topics)
    Ndw <- table_1d_fast(train.dev$train[, 'doc'], train.dev$n.docs)
    
    alpha <- .01
    beta <- .05
    
    perplexity <- function()
    {
      theta_dk <- apply(Ndk + alpha, 2, '/', Ndw + n.topics * alpha)
      phi_kw <- apply(Nwk + beta, 1, '/', Nk + train.dev$n.words * beta)
      
      p_wk <- theta_dk[train.dev$dev[,'doc'] + 1, ] *
        t(phi_kw[, train.dev$dev[,'word'] + 1])
      
      p_w <- apply(p_wk, 1, FUN = sum)
      
      log_pw <- log(p_w)
      
      res <- exp(-sum(log_pw)/nrow(train.dev$dev))
      return(res)
    }
    
    Z0 <- Z
    
    niter <- 20 # thin factor
    perp.res <- c()
    doc.trace <- c()
    word.trace <- c()
    nk.trace <- c()
    for (i in 1:n.save)
    {
      gibbs.res <- gibbs_lda_c(train.dev$train,
                               Ndk, Nwk, Nk, Z, 
                               train.dev$doc.trace.list, train.dev$word.trace.list, 
                               n.topics, niter, beta, alpha)
      
      doc.trace <- abind(doc.trace, gibbs.res$doc.trace, along = 3)
      word.trace <- abind(word.trace, gibbs.res$word.trace, along = 3)
      nk.trace <- cbind(nk.trace, gibbs.res$nk.trace)
      perp.res <- cbind(perp.res, perplexity())
    }
    save(perp.res, 
         word.trace, 
         doc.trace, 
         train.dev, 
         Z0, Z, seed0, niter, n.topics, n.save, repi,
         file = save.fn)
  }
  #plot(perp.res)
  return('success')
}
# n.save <- 100
# K <- 20
# repi <- 1
# 
# train.dev <- data.split(doc.pairs, .2)
# sourceCpp('gibbsLda/src/gibbs.cpp')
# res <- gibbs.lda(train.dev, K, n.save, repi)

# n.rep <- 10
# K.set <- seq(from = 10, to = 100, by = 1)
# ratio.set <- seq(from = .1, to = .9, by = .1)

n.rep <- 5
K.set <- c(10, 20, 30, 40, 50)
ratio.set <- c(.2, .3, .5, .8)
n.save <- 1000

params.grid <- expand.grid(ratio.set, K.set, 1:n.rep)

writeLines(c(""), "log.txt")

cl <- makePSOCKcluster(12)
registerDoParallel(cl)

perp.res.all <- foreach(
  ratio = params.grid[, 1],
  K = params.grid[, 2],
  repi = params.grid[, 3], 
  .packages = c('abind', 'gibbsLda')) %dopar% {
    
    sink("log.txt", append=TRUE)
    
    cat(ratio, K, repi, paste(Sys.time()), '\n')
    train.dev <- data.split(doc.pairs, ratio)
    
    tryCatch(
      res <- gibbs.lda(train.dev, K, n.save, repi),
      error = function(e) {
        res <<- paste('error', e)
      },
      finally = cat(ratio, K, repi, paste(Sys.time()), '\n')
    )
    res
  }

save(perp.res.all, file = '/scratch/yli-data/bst512/results-2.rdb')

stopCluster(cl)
