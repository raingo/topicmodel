#setwd('/home/raingo/workplace/BST512/project/code')
#setwd('/u/yli/workplace/bst512/')

library(abind)

gibbs.lda.save.fn <- function(n.topics, n.save, repi, train.ratio, data.name)
{
  save.fn <- paste(base.dir,
                   'perp.res-',
                   data.name,'-',
                   n.topics, '-',
                   n.save, '-',
                   repi, '-',
                   train.ratio, '.rdb', sep='')
  return(save.fn)
}

source('load.data.R')

gibbs.lda <- function(train.dev, n.topics, n.save = 100, repi = 0, data.name = 'general')
{
    save.fn <- gibbs.lda.save.fn(n.topics, n.save, repi, train.dev$train.ratio, data.name)
    if(file.exists(save.fn) && T)
    {
        cat('loading existing data', '\n')
        load(save.fn)
    }
    else{
        seed0 <- as.integer(Sys.time())
        set.seed(seed0)

        Z <- as.integer(sample.int(n.topics, size = nrow(train.dev$train), replace = T)) - 1L

        Ndk <- table_2d_fast(train.dev$train[, 'doc'], Z, train.dev$n.docs, n.topics)
        Nwk <- table_2d_fast(train.dev$train[, 'word'], Z, train.dev$n.words, n.topics)
        Nk <- table_1d_fast(Z, n.topics)
        Ndw <- table_1d_fast(train.dev$train[, 'doc'], train.dev$n.docs)

        alpha <- .01
        beta <- .05

        eval.bias <- F

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

        # cosine distance over rows of matrix
        cosine <- function(x) {
            y <- x %*% t(x)
            mag <- diag(y)
            res <- 1 - y / mag
            return(res)
        }


        if (!is.null(train.dev$trueTheta))
        {
            eval.bias <- T
            true.dist <- as.vector(cosine(train.dev$trueTheta))
        }

        pseudo.bias <- function()
        {
            theta_dk <- apply(Ndk + alpha, 2, '/', Ndw + n.topics * alpha)
            pred.dist <- as.vector(cosine(theta_dk))
            #res <- sum((true.dist - pred.dist)^2)
            res <- cor(pred.dist, true.dist)
            return(res)
        }

        Z0 <- Z

        # to make the copy actual happen
        Z0 <- Z0 + 0L

        niter <- 20 # thin factor
        perp.res <- c()
        doc.trace <- c()
        word.trace <- c()
        nk.trace <- c()
        bias.res <- c()
        for (i in 1:n.save)
        {
            gibbs.res <- gibbs_lda_c(
                                     train.dev$train,
                                     Ndk, Nwk, Nk, Z,
                                     train.dev$doc.trace.list,
                                     train.dev$word.trace.list,
                                     n.topics, niter, beta, alpha
                                     )

            doc.trace <- abind(
                               doc.trace,
                               gibbs.res$doc.trace,
                               along = 3
                               )
            word.trace <- abind(
                                word.trace,
                                gibbs.res$word.trace,
                                along = 3
                                )
            nk.trace <- cbind(nk.trace, gibbs.res$nk.trace)
            perp.res <- c(perp.res, perplexity())

            if (eval.bias)
                bias.res <- c(bias.res, pseudo.bias())
        }
        save(perp.res,
             word.trace,
             doc.trace, nk.trace,
             train.dev, bias.res,
             Z0, Z, seed0, niter, n.topics, n.save, repi,
             file = save.fn)
    }
    return(paste('success', save.fn))
}
