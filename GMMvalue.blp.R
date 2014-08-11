GMMvalue.blp <- function(thet){
  
  delta <- conmap.blp(thet)
  delta <- delta[[1]]
  
  objective <- objective.blp(thet, delta)
  objective <- objective[[4]]
  objective
  
}