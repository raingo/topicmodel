#base.dir <- '/u/yli/yli-data/bst512/'
#base.dir <- '/scratch/yli-data/bst512/'
base.dir <- 'data/'
n.core <- 2

if (!file.exists(base.dir))
  dir.create(base.dir)