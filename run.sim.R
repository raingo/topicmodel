library(doParallel)
source('load.data.R')
source('gibbs.parallel.R')

n.save <- 100

rep <- 5

numTopics <- 50
sizeVocab <- 10000
numDocs <- 5000
averageDocumentLength <- 100
averageWordsPerTopic <- 10

# repi.d <- 2
for (repi.d in 1:rep)
{
  dataset <- load.sim.data(numTopics,
                           sizeVocab,
                           numDocs,
                           averageDocumentLength,
                           averageWordsPerTopic, repi.d)

  gibbs.parallel(dataset, n.save)
}

rep <- 5

numTopics <- 5
sizeVocab <- 1000
numDocs <- 500
averageDocumentLength <- 100
averageWordsPerTopic <- 10

# repi.d <- 2
for (repi.d in 1:rep)
{
  dataset <- load.sim.data(numTopics,
                           sizeVocab,
                           numDocs,
                           averageDocumentLength,
                           averageWordsPerTopic, repi.d)

  gibbs.parallel(dataset, n.save)
}

#stop('enough')


# n.save <- 100
# K <- 5
# repi <- 1
#
# train.dev <- data.split(dataset, .8)
# sourceCpp('gibbsLda/src/gibbs.cpp')
# res <- gibbs.lda(train.dev, K, n.save, repi, dataset$digest)
