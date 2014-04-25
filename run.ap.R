
source('load.data.R')
source('gibbs.parallel.R')

dataset <- load.ap.data()
gibbs.parallel(dataset)