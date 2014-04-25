library(doParallel)
source('gibbs.R')
gibbs.parallel <- function(dataset)
{
  n.rep <- 10
  K.set <- seq(from = 10, to = 100, by = 1)
  ratio.set <- seq(from = .1, to = .9, by = .1)
  
  n.rep <- 5
  K.set <- c(10, 20, 30, 40, 50)
  ratio.set <- c(.2, .3, .5, .8)
  n.save <- 1000
  
  params.grid <- expand.grid(ratio.set, K.set, 1:n.rep)
  
  log.fn <- paste("log-", dataset$digest, ".txt")
  writeLines(c(""), log.fn)
  
  cl <- makePSOCKcluster(n.core)
  registerDoParallel(cl)
  
  perp.res.all <- foreach(
    ratio = params.grid[, 1],
    K = params.grid[, 2],
    repi = params.grid[, 3], 
    .packages = c('abind', 'gibbsLda')) %dopar% {
      
      sink(log.fn, append=TRUE)
      
      cat(ratio, K, repi, paste(Sys.time()), '--begin--\n')
      train.dev <- data.split(dataset, ratio)
      
      tryCatch(
        res <- gibbs.lda(train.dev, K, n.save, repi),
        error = function(e) {
          res <<- paste('error', e)
        },
        finally = cat(ratio, K, repi, paste(Sys.time()), '--end--\n')
      )
      res
    }
  
  save(perp.res.all, file = paste(base.dir, 'results-', dataset$digest, '-.rdb', sep = ''))
  
  stopCluster(cl)
}
