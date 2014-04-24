library(Rcpp)
sourceCpp('gibbsLda/src/gibbs.cpp')

doc.pairs <- as.matrix(read.table('ap-pairs.dat'))
colnames(doc.pairs) <- c('doc', 'word', 'cnt')
storage.mode(doc.pairs) <- 'integer'

doc.pairs <- doc.pairs[1:1000, ]