#' Calculate Benati and Capurri's (2026) alignment index
#'
#' @description
#'
#' \code{bcai()} takes two vectors and returns Benati and Capurri's (2026)
#' alignment index.
#'
#' @return
#'
#' \code{bcai()} takes two vectors and returns Benati and Capurri's (2026)
#' alignment index.
#'
#' @details
#'
#' You can think of the alignment index that Benati and Capurri (2026) describe
#' as an *S* corollary to the chance-corrected measures that Häge (2011) offers
#' as substitutes for *S*. It takes the (unweighted, absolute distances) *S*
#' score proposed by Signorino and Ritter (1999) and subtracts from it the *S*
#' score that would follow under the assumption of independent voting.
#'
#' The function subsets to complete cases of the two vectors for which you want
#' an alignment score.
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
#' bcai(gmyrus14$gmy, gmyrus14$rus, levels = 0:3) # with levels argument
#' bcai(bencapex$rowv, bencapex$colv) # levels argument not necessary here.
#'
#' @references
#'
#' Benati, Stefano, and Agnese Capurri. 2026. "The Alignment index and its
#' application to voting at the United Nations General Assembly." *Quality &
#' Quantity*. \doi{10.1007/s11135-026-02814-x}
#'
#' @importFrom stats complete.cases
#' @export

bcai <- function(x1, x2, levels = NULL) {

  if(length(x1) != length(x2)) {
    stop("`x1` and `x2` are not the same length.")
  }

  if (is.null(levels)) {

    use.these.levels <- sort(unique(c(x1, x2)))

  } else {

    use.these.levels <- levels

  }

  # for complete cases...
  completetf <- complete.cases(x1, x2)

  x1 <- x1[completetf]
  x2 <- x2[completetf]

  # Calculate unweighted, absolute values S first...
  tab <- table(factor(x1, levels = use.these.levels),
               factor(x2, levels = use.these.levels))

  o <- prop.table(tab)

  rmarg <- rowSums(o)
  cmarg <- colSums(o)

  e <- outer(rmarg, cmarg)
  e

  d <- abs(row(o) - col(o))
  dd <- nrow(o) - 1

  S <- 1 - 2*sum(o*d)/dd
  S

  # Calculate *S* under conditions of jointly independent voting...
  e <- outer(rmarg, cmarg)

  ed <- abs(row(e) - col(e))
  edd <- nrow(e) - 1

  E <- 1 - 2*sum(e*ed)/edd
  E

  a <- S-E

  return(a)

}
