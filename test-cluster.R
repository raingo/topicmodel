
library(doSNOW)

getnodes <- function(hosts) {
    #f <- Sys.getenv('PBS_NODEFILE')
    #x <- if (nzchar(f)) readLines(f) else rep('localhost', 3)
    x <- hosts
    as.data.frame(table(x), stringsAsFactors=FALSE)
}

hosts <- c('cycle1', 'cycle1', 'cycle3', 'cycle2', 'cycle3')
nodes <- getnodes(hosts)
cl <- makeSOCKcluster(nodes$x)
registerDoSNOW(cl)

setcores <- function(cl, nodes) {
    f <- function(cores) assign('allocated.cores', cores, pos=.GlobalEnv)
    clusterApply(cl, nodes$Freq, f)
}
setcores(cl, nodes)

r <- foreach(i=seq_along(nodes$x), .packages='doMC') %dopar% {
    registerDoMC(allocated.cores)
    ppid <- Sys.getpid()
    foreach(j=1:allocated.cores, .combine='c', .packages='R.utils') %dopar% {
        Sys.sleep(1)
        c(ppid, Sys.getpid(), System$getHostname())
    }
}
stopCluster(cl)
