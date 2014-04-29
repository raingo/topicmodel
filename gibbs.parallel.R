#library(doParallel)
library(doSNOW)
source('gibbs.R')
source('load.data.R')

sampling.params.grid <- function()
{
    n.rep <- 5
    #K.set <- c(10, 20, 30, 40, 50)
    K.set <- seq(from = 100, to = 10, by = -10)
    ratio.set <- seq(from = .9, to = .1, by = -.1)
    #ratio.set <- c(.2, .3, .5, .8)
    params.grid <- expand.grid(ratio.set, K.set, n.rep:1)
    #params.grid <- expand.grid(ratio.set, K.set)
    colnames(params.grid) <- c('ratio', 'K', 'repi')
    return(params.grid)
}

gibbs.parallel <- function(dataset, n.save = 1000)
{
    cat(dataset$digest, '\n')
    log.fn <- paste("logs/log-", dataset$digest, ".txt", sep = "")

    params.grid <- sampling.params.grid()

    writeLines(c(""), log.fn)

    getnodes <- function() {
        f <- Sys.getenv('PBS_NODEFILE')
        x <- if (nzchar(f)) readLines(f) else rep('localhost', 3)
        #x <- hosts
        #as.data.frame(table(x), stringsAsFactors=FALSE)
        return(x)
    }
    nodes <- getnodes()
    cl <- makeSOCKcluster(nodes)
    registerDoSNOW(cl)

    cwd <- getwd()
    cat(cwd, '\n')

    perp.res.all <- foreach(
                            ratio = params.grid[, 'ratio'],
                            K = params.grid[, 'K'],
                            repi = params.grid[, 'repi'],
                            .packages = c('abind', 'gibbsLda')
                            ) %dopar% {
        setwd(cwd)

        source('load.data.R')
        source('gibbs.R')

        cat(ratio, K, repi, paste(Sys.time()), '--begin--\n', file = log.fn, append = T)
        train.dev <- data.split(dataset, ratio)

        tryCatch(
                 res <- gibbs.lda(train.dev, K, n.save, repi, dataset$digest),
                 error = function(e) {
                     cat('error: ', e, '\n')
                     res <<- paste('error', e)
                 },
                 finally = cat(ratio, K, repi, paste(Sys.time()), '--end--\n', file = log.fn, append = T)
                 )
        res
    }

    save(perp.res.all, file = paste(base.dir, 'results-', dataset$digest, '.rdb', sep = ''))

    stopCluster(cl)
}
