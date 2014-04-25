
source('gibbs.parallel.R')

gibbs.vis.stat <- function(dataset)
{
  params.grid <- sampling.params.grid()
  for(i in 1:nrow(params.grid))
  {
    ratio <- params.grid[i, 'ratio'],
    K <- params.grid[i, 'K'],
    repi <- params.grid[i, 'rep']
    
    
  }
}