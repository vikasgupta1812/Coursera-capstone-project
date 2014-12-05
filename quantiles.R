## calculate quantiles, quick and dirty method. We have already a probability distribution, 
## in a vector, sorted decreasingly by probability. Loop through the elements, stop when
## the desired quantile is achieved. 

calculateQuantile <- function(distribution, quantile) {
  ## careful, no param checking here. 
  
  prob <- 0
  
  for (i in 1:length(distribution)) {
    prob <- prob + distribution[i]
    if (prob > quantile){
      print(prob)
      return(i)
    }
  }
}