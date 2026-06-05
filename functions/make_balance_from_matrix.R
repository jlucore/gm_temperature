make_balance_from_matrix <- function(matrix, numerator, denominator) {
  
  num <- intersect(numerator, colnames(matrix))
  den <- intersect(denominator, colnames(matrix))
  stopifnot(length(num) > 0, length(den) > 0)
  
  nmean <- rowMeans(log(matrix[, num, drop = FALSE]))
  dmean <- rowMeans(log(matrix[, den, drop = FALSE]))
  nmean - dmean
}