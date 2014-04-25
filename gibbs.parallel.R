library(doParallel)
source('gibbs.R')
source('load.data.R')

sampling.params.grid <- function()
{
  n.rep <- 5
  K.set <- c(10, 20, 30, 40, 50)
  ratio.set <- c(.2, .3, .5, .8)
  params.grid <- expand.grid(ratio.set, K.set, 1:n.rep)
  colnames(params.grid) <- c('ratio', 'K', 'rep')
  return(params.grid)
}
gibbs.parallel <- function(dataset)
{
  n.save <- 1000

  log.fn <- paste("logs/log-", dataset$digest, ".txt", sep = "")

  params.grid <- sampling.params.grid()

  writeLines(c(""), log.fn)

  cl <- makePSOCKcluster(n.core)
  registerDoParallel(cl)

  perp.res.all <- foreach(
    ratio = params.grid[, 'ratio'],
    K = params.grid[, 'K'],
    repi = params.grid[, 'rep'],
    .packages = c('abind', 'gibbsLda')) %dopar% {
      source('load.data.R')
      source('gibbs.R')

      sink(log.fn, append=TRUE)

      cat(ratio, K, repi, paste(Sys.time()), '--begin--\n')
      train.dev <- data.split(dataset, ratio)

      tryCatch(
        res <- gibbs.lda(train.dev, K, n.save, repi, dataset$digest),
        error = function(e) {
            cat('error: ', e, '\n')
          res <<- paste('error', e)
        },
        finally = cat(ratio, K, repi, paste(Sys.time()), '--end--\n')
      )
      res
    }

  save(perp.res.all, file = paste(base.dir, 'results-', dataset$digest, '.rdb', sep = ''))

  stopCluster(cl)
}
