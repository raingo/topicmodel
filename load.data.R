
library(digest)
source('local_settings.R')

load.ap.data <- function()
{
  save.name <- 'ap-pairs.dat'
  doc.pairs <- as.matrix(read.table(save.name))
  colnames(doc.pairs) <- c('doc', 'word', 'cnt')
  storage.mode(doc.pairs) <- 'integer'

  vocab <- as.matrix(read.table('vocab.txt'))

  dataset <- list()
  dataset$triple.let <- doc.pairs
  dataset$vocab <- vocab
  dataset$digest <- digest(save.name)

  return(dataset)
}

data.split <- function(dataset, train.ratio = .8)
{
  triple.let <- dataset$triple.let
  train.dev <- runif(nrow(triple.let)) < train.ratio

  if('trueTheta' %in% dataset)
  {
    n.words <- ncol(dataset$truePhi)
    n.docs <- nrow(dataset$trueTheta)
  } else
  {
    n.words <- max(triple.let[, 2]) + 1L
    n.docs <- max(triple.let[, 1]) + 1L
  }

  word.candi <- unique(triple.let[, 'word'])
  word.trace.n <- 8L
  word.trace.list <- as.integer(sample(word.candi, size = word.trace.n, replace = F))

  doc.candi <- unique(triple.let[, 'doc'])
  doc.trace.n <- 8L
  doc.trace.list <- as.integer(sample(doc.candi, size = doc.trace.n, replace = F))

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
              'doc.trace.list' = doc.trace.list,
              'truePhi' = dataset$truePhi,
              'trueTheta' = dataset$trueTheta,
              'vocab' = dataset$vocab)
  return(res)
}

sim.data.save.fn <- function(numTopics,
                             sizeVocab,
                             numDocs,
                             averageDocumentLength,
                             averageWordsPerTopic, repi)
{
  save.fn <- paste(base.dir, 'lda-',
                   numTopics, '-',
                   sizeVocab, '-',
                   numDocs, '-',
                   averageDocumentLength, '-',
                   averageWordsPerTopic, '-', repi, '.rdb', sep='')
  return(save.fn)
}

load.sim.data <- function(numTopics,
                          sizeVocab,
                          numDocs,
                          averageDocumentLength,
                          averageWordsPerTopic, repi)
{
  save.fn <- sim.data.save.fn(numTopics,
                              sizeVocab,
                              numDocs,
                              averageDocumentLength,
                              averageWordsPerTopic, repi)
  load(save.fn)

  dataset$digest <- digest(basename(save.fn))
  return(dataset)
}
