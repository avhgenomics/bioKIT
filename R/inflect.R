

#' Inflect
#'
#' Finds local minima and maxima in a vector.
#'
#' @param x The vector to search.
#' @param threshold The sensitivity; default is 1.
#'
#' @return Returns a list containing the minima and maxima.
#' @export
#' @examples
#' # ADD_EXAMPLES_HERE
inflect <- function(x, threshold = 1){
  up   <- sapply(1:threshold, function(n) c(x[-(seq(n))], rep(NA, n)))
  down <-  sapply(-1:-threshold, function(n) c(rep(NA,abs(n)), x[-seq(length(x), length(x) - abs(n) + 1)]))
  a    <- cbind(x,up,down)
  list(minima = which(apply(a, 1, min) == a[,1]), maxima = which(apply(a, 1, max) == a[,1]))
}
