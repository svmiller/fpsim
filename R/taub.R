#' Calculate Kendall's (1938) Tau-B
#'
#' @description
#'
#' \code{taub()} takes two vectors and returns Kendall's Tau-b as a measure of
#' rank correlation.
#'
#' @return
#'
#' \code{taub()} takes two vectors and returns Kendall's Tau-b as a measure of
#' rank correlation.
#'
#' @details
#'
#' I'll be honest that I wrote this just to say that I did write this and that
#' my workflow would still lean on using the [cor()] function in base R.
#'
#'
#'
#' @param x1 a vector, and one assumes an integer
#' @param x2 a vector, and one assumes an integer
#'
#' @examples
#'
#' taub(usamex46$vote1, usamex46$vote2)
#' taub(gmyrus14$gmy, gmyrus14$rus)
#'
#' # Compare with...
#'
#' cor(usamex46$vote1, usamex46$vote2, method = 'kendall')
#' cor(gmyrus14$gmy, gmyrus14$rus, method = 'kendall')
#'
#' @references
#'
#' Kendall, Maurice G. 1938. "A New Measure of Rank Correlation". *Biometrika*
#' 30(1/2): 81-93.
#'
#' @importFrom stats complete.cases
#' @export

taub <- function(x1, x2) {

  if(length(x1) != length(x2)) {

    stop("`x1` and `x2` are not the same length.")
  }


  # force complete cases, just in case
  completetf <- complete.cases(x1, x2)

  x1 <- x1[completetf]
  x2 <- x2[completetf]

  n <- length(x1)
  if (n < 2) {

    stop("taub() needs at least two complete observations.")

  }

  con <- dis <- tx1 <- tx2 <- 0

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {

      dx1 <- x1[i] - x1[j]
      dx2 <- x2[i] - x2[j]

      if (dx1 == 0 && dx2 == 0) {
        next  # identical points ignored, per Kendall
      } else if (dx1 == 0) {
        tx1 <- tx1 + 1
      } else if (dx2 == 0) {
        tx2 <- tx2 + 1
      } else if (dx1*dx2 > 0) {
        con <- con + 1
      } else if (dx1*dx2  < 0) {
        dis <- dis + 1
      }
    }
  }

  denom <- sqrt((con + dis + tx1)*(con + dis + tx2))

  if (denom == 0) return(NA)  # undefined when denominator is 0

  tb <- (con - dis)/denom

  return(tb)

}
