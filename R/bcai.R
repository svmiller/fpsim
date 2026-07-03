#' Calculate Benati and Capurri's (2026) alignment index (A)
#'
#' @description
#'
#' \code{bcai()} takes two vectors and returns Benati and Capurri's (2026)
#' alignment index (A).
#'
#' @return
#'
#' \code{bcai()} takes two vectors and returns Benati and Capurri's (2026)
#' alignment index (A).
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
#' ## A Few Caveats on Weighting
#'
#' You can weight this measure if you want. Please be mindful about what you're
#' doing, especially if the weights are CINC scores. See here:
#'
#' \url{https://svmiller.com/blog/2026/06/alliances-weighting-foreign-policy-similarity/}
#'
#' The function will proportionalize your weights to sum to 1 if they do not
#' already.
#'
#'
#'
#' @param x1 a vector, and one assumes an integer
#' @param x2 a vector, and one assumes an integer
#' @param distances the type of distances between ratings/attachments to
#' estimate. Can be either "absolute" or "squared". Defaults to "absolute", but
#' see note in details section.
#' @param weights a vector of weights. Defaults to NULL for creating unweighted
#' A index values.
#' @param levels defaults to NULL, but an optional vector that defines the full
#' sequence of values that could be observed in `x1` and `x2`. If NULL, the
#' function looks for observed values.
#'
#' @examples
#'
#' bcai(gmyrus14$gmy, gmyrus14$rus, levels = 0:3) # with levels argument
#' bcai(bencapex$rowv, bencapex$colv) # levels argument not necessary here.
#' bcai(gmyrus14$gmy, gmyrus14$rus, distances = 'squared', levels = 0:3) # squared, with levels argument
#'
#' @references
#'
#' Benati, Stefano, and Agnese Capurri. 2026. "The Alignment index and its
#' application to voting at the United Nations General Assembly." *Quality &
#' Quantity*. \doi{10.1007/s11135-026-02814-x}
#'
#' @importFrom stats complete.cases xtabs
#' @export

bcai <- function(x1, x2, distances = "absolute", weights = NULL, levels = NULL) {

  if(length(x1) != length(x2)) {
    stop("`x1` and `x2` are not the same length.")
  }

  if (!is.null(weights) && (length(weights) != length(x1) || length(weights) != length(x2))) {
    stop("`weights` must be the same length as `x1` and `x2` if you're going to provide it.")
  }

  if (is.null(levels)) {

    use.these.levels <- sort(unique(c(x1, x2)))

  } else {

    use.these.levels <- levels

  }

  if(is.null(weights)) {
    completetf <- complete.cases(x1, x2)

    x1 <- x1[completetf]
    x2 <- x2[completetf]

  } else {

    completetf <- complete.cases(x1, x2, weights)

    x1 <- x1[completetf]
    x2 <- x2[completetf]
    weights <- weights[completetf]

  }

## 1. Absolute Distances ----
  if(distances == "absolute") {


    if(is.null(weights)) {
      ## * Absolute Distances, No Weights -----

      # Calculated observed S...
      tab <- table(factor(x1, levels = use.these.levels),
                   factor(x2, levels = use.these.levels))

      o <- prop.table(tab)

      d <- abs(row(o) - col(o))
      dd <- max(use.these.levels) - min(use.these.levels) # nrow(o) - 1 # nrow(o) - 1

      S <- 1 - 2*sum(o*d)/dd

      # Calculate expected S under conditions of jointly independent voting...
      rmarg <- rowSums(o)
      cmarg <- colSums(o)
      e <- outer(rmarg, cmarg)

      ed <- abs(row(e) - col(e))
      edd <-  nrow(e) - 1

      E <- 1 - 2*sum(e*ed)/edd



    } else { # Okay, so you have weights... cue Shania Twain...

      ## * Absolute Distances, with Weights ----

      # Calculate observed S, with weights...
      o <- xtabs(weights ~
                   factor(x1, levels = use.these.levels) +
                   factor(x2, levels = use.these.levels))

      # Check if o sums to 1... it must...

      if(sum(o) != 1) {
        o <- o/sum(o)
      }

      rmarg <- rowSums(o)
      cmarg <- colSums(o)

      d  <- abs(row(o) - col(o))
      dd <- max(use.these.levels) - min(use.these.levels) # nrow(o) - 1

      S <- 1 - 2 * sum(o * d) / dd

      # Calculate expected S, with weights...
      e <- outer(rmarg, cmarg)

      ed <- abs(row(e) - col(e))
      edd <- max(use.these.levels) - min(use.these.levels) # nrow(e) - 1

      E <- 1 - 2*sum(e*ed)/edd

    }

  }

  ## 2. Squared Distances ----
  if(distances == "squared") {

    if(is.null(weights)) {
      ## * Squared Distances, No Weights -----

      # Calculated observed S...
      tab <- table(factor(x1, levels = use.these.levels),
                   factor(x2, levels = use.these.levels))

      o <- prop.table(tab)

      d <- abs(row(o) - col(o))^2
      dd <- (max(use.these.levels) - min(use.these.levels))^2 # nrow(o) - 1

      S <- 1 - 2*sum(o*d)/dd

      # Calculate expected S under conditions of jointly independent voting...
      rmarg <- rowSums(o)
      cmarg <- colSums(o)
      e <- outer(rmarg, cmarg)

      ed <- abs(row(e) - col(e))^2
      edd <- (max(use.these.levels) - min(use.these.levels))^2 # nrow(e) - 1

      E <- 1 - 2*sum(e*ed)/edd


    } else { # Okay, so you have weights... cue Shania Twain...
      ## * Squared Distances, With Weights -----

      # Calculate observed S, with weights...
      o <- xtabs(weights ~
                   factor(x1, levels = use.these.levels) +
                   factor(x2, levels = use.these.levels))

      # Check if o sums to 1... it must...

      if(sum(o) != 1) {
        o <- o/sum(o)
      }

      rmarg <- rowSums(o)
      cmarg <- colSums(o)

      d  <- abs(row(o) - col(o))^2
      dd <- (max(use.these.levels) - min(use.these.levels))^2 # nrow(o) - 1

      S <- 1 - 2 * sum(o * d) / dd

      # Calculate expected S, with weights...
      e <- outer(rmarg, cmarg)

      ed <- abs(row(e) - col(e))^2
      edd <- (max(use.these.levels) - min(use.these.levels))^2 # nrow(e) - 1

      E <- 1 - 2*sum(e*ed)/edd

    }

  }

  a <- S-E

  return(a)

}
