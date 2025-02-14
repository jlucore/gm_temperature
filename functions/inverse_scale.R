inverse_scale <- function(x) {
  return(round(x * 2 * sd_temp + mean_temp))  # reverse scale in figure axes
}