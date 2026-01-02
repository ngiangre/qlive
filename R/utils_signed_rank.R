signed_rank <- function(x) {
  sign(x) * rank(abs(x))
}
