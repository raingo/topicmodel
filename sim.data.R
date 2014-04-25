library(MCMCpack)

# translate from C# code of 
# http://research.microsoft.com/en-us/um/cambridge/projects/infernet/docs/Latent%20Dirichlet%20Allocation.aspx

CreateTrueThetaAndPhi <- function(numVocab, numTopics, numDocs, averageDocLength, averageWordsPerTopic)
{
  truePhi <- matrix(0, nrow = numTopics, ncol = numVocab)
  for (i in 1:numTopics)
  {
    # generate number of words for each topic 
    numUniqueWordsPerTopic <- min(1 + rpois(1, averageWordsPerTopic), numVocab)
    
    # generate the actual words
    wordIndex <- sample.int(numVocab, numUniqueWordsPerTopic, replace = F)
    
    # generate word count for each word
    expectedRepeatOfWordInTopic <- numDocs * averageDocLength / numUniqueWordsPerTopic
    wordCnt <- rpois(numUniqueWordsPerTopic, expectedRepeatOfWordInTopic)
    
    truePhi[i, wordIndex] <- wordCnt
  }
  
  trueTheta <- matrix(0, nrow = numDocs, ncol = numTopics)
  
  for(i in 1:numDocs)
  {
    # generate number of topics
    numUniqueTopicsPerDoc <- min(1 + rpois(1, 1.0), numTopics)
    
    # generate actual topics
    topicIndex <- sample.int(numTopics, numUniqueTopicsPerDoc, replace = F)
    
    # generate topic count for each topic
    expectedRepeatOfTopicInDoc <- 
      averageDocLength / numUniqueTopicsPerDoc;
    topicCnt <- rpois(numUniqueTopicsPerDoc, expectedRepeatOfTopicInDoc)
    
    trueTheta[i, topicIndex] <- topicCnt
  }
  
  res <- list('trueTheta' = trueTheta, 'truePhi' = truePhi)
  return(res)
}

GenerateLDAData <- function(trueTheta, truePhi, averageNumWords)
{
  numVocab <- ncol(truePhi) 
  numTopics <- nrow(truePhi)
  numDocs <- nrow(trueTheta)
  
  topicDist <- t(sapply(1:nrow(trueTheta), FUN = function(i) rdirichlet(1, trueTheta[i, ])))
  wordDist <- t(sapply(1:nrow(truePhi), FUN = function(i) rdirichlet(1, truePhi[i, ])))
  
  triple.let <- c()
  for (i in 1:numDocs)
  {
    LengthOfDoc <- rpois(1, averageNumWords);
    topics <- sapply(
      1:LengthOfDoc, 
      FUN = function(...) 
        sample(numTopics, size = 1, prob = topicDist[i, ])
    )
    
    words <- sapply(
      topics,
      FUN = function (topic) 
        sample(numVocab, size = 1, prob = wordDist[topic, ])
    )
    triple.let <- rbind(
      triple.let, 
      cbind(rep(i - 1, LengthOfDoc), words - 1, rep(1, LengthOfDoc))
    )
  }
  
  triple.let <- as.matrix(triple.let)
  colnames(triple.let) <- c('doc', 'word', 'cnt')
  storage.mode(triple.let) <- 'integer'
  return(triple.let)
}

gen.dateset <- function(
  numTopics, 
  sizeVocab, 
  numDocs, 
  averageDocumentLength, 
  averageWordsPerTopic, repi
)
{
  save.fn <- paste('data/lda-',
                   numTopics, '-',
                   sizeVocab, '-',
                   numDocs, '-',
                   averageDocumentLength, '-',
                   averageWordsPerTopic, '-', repi, '.rdb')
  
  if (file.exists(save.fn))
  {
    load(save.fn)
    return(dataset)
  }
  
  model.gt <- CreateTrueThetaAndPhi(
    sizeVocab, numTopics, numDocs, averageDocumentLength, averageWordsPerTopic)
  
  # Generate training and test data for the training documents
  triple.let <- GenerateLDAData(
    model.gt$trueTheta, 
    model.gt$truePhi, 
    averageDocumentLength
  )
  
  dataset <- list()
  dataset$trueTheta <- model.gt$trueTheta
  dataset$truePhi <- model.gt$truePhi
  dataset$triple.let <- triple.let
  
  save(dataset, file = save.fn)
  return(dataset)
}

rep <- 5

numTopics <- 50
sizeVocab <- 10000
numDocs <- 5000
averageDocumentLength <- 100
averageWordsPerTopic <- 10

for(repi in 1:rep)
{
  gen.dateset(numTopics, 
              sizeVocab, 
              numDocs, 
              averageDocumentLength, 
              averageWordsPerTopic, repi)
}

rep <- 5

numTopics <- 5
sizeVocab <- 1000
numDocs <- 500
averageDocumentLength <- 100
averageWordsPerTopic <- 10

for(repi in 1:rep)
{
  gen.dateset(numTopics, 
              sizeVocab, 
              numDocs, 
              averageDocumentLength, 
              averageWordsPerTopic, repi)
}

