#setwd('/home/raingo/workplace/BST512/project/code')
#setwd('/u/yli/workplace/bst512/')
library(doParallel)
library('gibbsLda')

doc.pairs <- as.matrix(read.table('ap-pairs.dat'))
colnames(doc.pairs) <- c('doc', 'word', 'cnt')
storage.mode(doc.pairs) <- 'integer'

vocab <- read.table('vocab.txt')

data.split <- function(triple.let, train.ratio = .8)
{
  train.dev <- runif(nrow(triple.let)) < train.ratio

  n.words <- max(triple.let[, 2]) + 1
  n.docs <- max(triple.let[, 1]) + 1

  train <- triple.let[train.dev, ]
  dev <- triple.let[!train.dev, ]

  res <- list('train' = train, 'dev' = dev, 'n.words' = n.words, 'n.docs' = n.docs, 'train.ratio' = train.ratio)
  return(res)
}

gibbs.lda <- function(train.dev, n.topics, n.save = 100, rep = 0)
{
    save.fn <- paste('/scratch/yli-data/bst512/', 'perp.res-', n.topics, '-', n.save, '-', rep, '-', train.dev$train.ratio, '.rdb', sep='')
    if(file.exists(save.fn))
    {
        cat('loading existing data')
        load(save.fn)
        return(prep.)
    }
    else{
        Z <- as.integer(sample.int(n.topics, size = nrow(train.dev$train), replace = T)) - 1

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

        niter <- 20 # thin factor
        perp_res <- array(dim = n.save)
        for (i in 1:n.save)
        {
            Z <- gibbs_lda_c(train.dev$train,
                             Ndk, Nwk, Nk, Z,
                             n.topics, niter, beta, alpha)
            perp_res[i] <- perplexity()
        }
        save(perp_res, file = save.fn)
    }
    #plot(perp_res)
    return(perp_res)
}

cl <- makePSOCKcluster(12)
registerDoParallel(cl)

# n.rep <- 10
# K.set <- seq(from = 10, to = 100, by = 1)
# ratio.set <- seq(from = .1, to = .9, by = .1)

n.rep <- 5
K.set <- c(10, 20, 30, 40, 50)
ratio.set <- c(.2, .3, .5, .8)

params.grid <- expand.grid(ratio.set, K.set, 1:n.rep)
n.save <- 1000

writeLines(c(""), "log.txt")

perp.res.all <- foreach(
                        ratio = params.grid[, 1],
                        K = params.grid[, 2],
                        rep = params.grid[, 3], .packages='gibbsLda') %dopar% {

    sink("log.txt", append=TRUE)

    cat(ratio, K, rep, paste(Sys.time()), '\n')
    train.dev <- data.split(doc.pairs, ratio)

    tryCatch(
             res <- gibbs.lda(train.dev, K, n.save, rep),
             error = function(e) {
                 res <<- paste('error', e)
             },
             finally = cat(ratio, K, rep, paste(Sys.time()), '\n')
             )
    res
}

save(perp.res.all, file = '/scratch/yli-data/bst512/results-2.rdb')

stopCluster(cl)
