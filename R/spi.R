#' Calculate Scott's (1955) pi
#'
#' @description
#'
#' \code{spi()} takes two vectors and returns Scott's (1955) pi coefficient,
#' communicating extent of inter-observer reliability.
#'
#' @return
#'
#' \code{spi()} takes two vectors and returns Scott's (1955) pi coefficient,
#' communicating extent of inter-observer reliability.
#'
#' @details
#' The function subsets to complete cases of the two vectors for which you want
#' Scott's pi.
#'
#' The function implicitly assumes that `x1` and `x2` are columns in a data
#' frame. One indirect check for this looks at whether `x1` and `x2` are the
#' same length. The function will stop if they're not.
#'
#' There will sometimes be instances, assuredly with alliances, where not all
#' categories are observed. For example, the toy example I provide of Germany
#' and Russia in 1914 includes no 2s. In the language of "ratings", the "rating"
#' of 2 was available for Germany and Russia in 1914 but neither side used it.
#' The `levels` argument allows you to specify the full sequence of values that
#' could be observed, even if none were. It probably makes the most sense to
#' always use this argument, even if the default behavior operates as if you
#' won't.
#'
#'
#'
#' @param x1 a vector, and one assumes an integer
#' @param x2 a vector, and one assumes an integer
#' @param levels defaults to NULL, but an optional vector that defines the full
#' sequence of values that could be observed in `x1` and `x2`. If NULL, the
#' function looks for observed values.
#'
#' @examples
#'
#' spi(gmyrus14$gmy, gmyrus14$rus, levels = 0:3) # with levels argument
#' spi(usamex46$vote1, usamex46$vote2) # levels argument not necessary here.
#'
#' @references
#'
#' Scott, William A. 1955. "Reliability of Content Analysis: The Case of Nominal
#' Scale Coding." *Public Opinion Quarterly* 19(3): 321–5.
#'
#' @importFrom stats complete.cases
#' @export

spi <- function(x1, x2, levels = NULL) {

  if(length(x1) != length(x2)) {
    stop("`x1` and `x2` are not the same length.")
  }

  if (is.null(levels)) {

    use_these_levels <- sort(unique(c(x1, x2)))

  } else {

    use_these_levels <- levels

  }

  # for complete cases...
  completetf <- complete.cases(x1, x2)

  x1 <- x1[completetf]
  x2 <- x2[completetf]

  tab <- table(factor(x1, levels = use_these_levels),
               factor(x2, levels = use_these_levels))

  # Total nobs
  n <- sum(tab)

  # Observed agreement (po)
  po <- sum(diag(tab))/n

  # Expected agreement (pe)
  # Scott’s pi assumes both raters/voters/classifiers/whatever draw from the same distribution
  marg <- rowSums(tab + t(tab))/(2*n)
  pe <- sum(marg^2)

  # Hi Scott
  pi <- (po - pe)/(1 - pe)

  return(pi)
}
