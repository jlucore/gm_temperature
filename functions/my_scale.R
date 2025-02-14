my_scale <- function(x){
  
  output <- (x - mean(x))/(2*sd(x))
  
}