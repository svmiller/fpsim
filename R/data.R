#' Select Alliance Portfolios of Germany and Russia, 1914
#'
#' A simple example of alliance portfolios of Germany and Russia, by way of
#' Signorino and Ritter's (1999) example.
#'
#' @format A data frame with 20 observations on the following 4 variables.
#' \describe{
#' \item{\code{state}}{a three-character code indicating a state}
#' \item{\code{syscap}}{the capabilities of the state, as a potential weight}
#' \item{\code{gmy}}{the alliance commitment for Germany with the state identified in the \code{state} column}
#' \item{\code{rus}}{the alliance commitment for Russia with the state identified in the \code{state} column}
#' }
#'
#' @details
#'
#' The data come by way of Signorino and Ritter (1999, Table 6).
#'
#' @references
#'
#' Signorino, Curtis S. and Jeffrey M. Ritter. "Tau-b or Not Tau-B: Measuring
#' the Similarity of Foreign Policy Positions." *International Studies Quarterly*
#' 43(1): 115–44.

"gmyrus14"

#' American-Mexican Dyadic Voting Patterns in the United Nations, 1946
#'
#' A simple example of voting patterns for the United States and Mexico in the
#' United Nations in 1946.
#'
#' @format A data frame with 38 observations on the following 6 variables.
#' \describe{
#' \item{\code{resid}}{an identifier for a roll-call vote ID}
#' \item{\code{ccode1}}{the Correlates of War state code for the United States (2)}
#' \item{\code{ccode2}}{the Correlates of War state code for Mexico (70)}
#' \item{\code{year}}{a numeric constant for the year (1946)}
#' \item{\code{vote1}}{an integer for how the United States voted on the resolution identified in the \code{resid} column}
#' \item{\code{vote2}}{an integer for how Mexico voted on the resolution identified in the \code{resid} column}
#' }
#'
#' @details
#'
#' Data are from a June 2024 of the United Nations voting data provided by
#' Erik Voeten on his Dataverse for the project.
#'
#' Valid vote values identified are 1 (yes), 2 (abstain), and 3 (no).
#'
#' @references
#'
#' Bailey, Michael A., Anton Strezhnev, and Erik Voeten. 2017. "Estimating
#' Dynamic State Preferences from United Nations Voting Data." *Journal of
#' Conflict Resolution* 61(2): 430-56.

"usamex46"


#' A Worked Example from Benati and Capurri (2026)
#'
#' A simple worked example illustrating what the authors call an "alignment
#' index." You can think of this as a kind of chance-corrected *S* score.
#'
#' @format A data frame with 240 observations on the following 2 variables.
#' \describe{
#' \item{\code{rowv}}{how the row voter voted on a resolution}
#' \item{\code{colv}}{how the column voter voted on a resolution}
#' }
#'
#' @details
#'
#' This is an expansion of Table 1 in their article. Valid vote values
#' identified are 1 (yes), 2 (abstain), and 3 (no).
#'
#' @references
#'
#' Benati, Stefano, and Agnese Capurri. 2026. "The Alignment index and its
#' application to voting at the United Nations General Assembly." *Quality &
#' Quantity*. \doi{10.1007/s11135-026-02814-x}

"bencapex"
