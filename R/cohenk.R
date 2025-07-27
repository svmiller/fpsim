#' Calculate Cohen's (1960, 1968) weighted kappa
#'
#' @description
#'
#' \code{cohenk()} takes two vectors and returns Cohen's kappa as an estimate
#' of chance-corrected agreement.
#'
#' @return
#'
#' \code{cohenk()} takes two vectors and returns Cohen's kappa as an estimate
#' of chance-corrected agreement.
#'
#' @details
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
#' @param x1 a vector, and one assumes an integer
#' @param x2 a vector, and one assumes an integer
#' @param w_exp an exponent to apply to the weight matrix. Default is 2 for
#' squared distances in the weight matrix. Supplying a 1 would make for linear
#' distances.
#' @param levels defaults to NULL, but an optional vector that defines the full
#' sequence of values that could be observed in `x1` and `x2`. If NULL, the
#' function looks for observed values.
#'
#' @examples
#'
#' cohenk(gmyrus14$gmy, gmyrus14$rus, levels = 0:3) # levels argument necessary
#' cohenk(usamex46$vote1, usamex46$vote2) # levels argument not necessary here.
#'
#' @references
#'
#' Cohen, Jacob. 1960. "A Coefficient of Agreement for Nominal Scales."
#' *Educational and Psychological Measurement* 20(1): 37-46.
#'
#' Cohen, Jacob. 1968. "Weighted Kappa: Nominal Scale Agreement with Provision
#' for Scaled Disagreement or Partial Credit." *Psychological Bulletin*
#' 70(4): 213--220.

cohenk <- function(x1, x2, w_exp = 2, levels = NULL) {

  if (is.null(levels)) {

    use_these_levels <- sort(unique(c(x1, x2)))

  } else {

    use_these_levels <- levels

  }

  tab <- table(factor(x1, levels = use_these_levels),
               factor(x2, levels = use_these_levels))
  o <- prop.table(tab)

  rmarg <- rowSums(o)
  cmarg <- colSums(o)

  e <- outer(rmarg, cmarg)

  rng <- max(use_these_levels) - min(use_these_levels)

  w <- outer(use_these_levels, use_these_levels, function(i, j) {
    d <- abs(i - j) / rng
    d <- d^w_exp
  })

  k <- 1 - (sum(w*o)/sum(w*e))

  return(k)

}

