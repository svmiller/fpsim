#' Download FPSIM Data
#'
#' @description
#'
#' \code{download.fpsim.data()} will download available pre-made dyadic foreign
#' policy similarity data and place its contents the package's \code{extdata/}
#' directory. It leverages R's \code{inst/} directory flexibility.
#'
#' @details
#'
#' This function is named in such a way to avoid a function clash with the
#' \code{download_extdata()} function in \pkg{peacesciencer}. It also comes
#' from my aversion to the use of underscores in R function. There is nothing
#' wrong with this convention, but the chatbots have ruined it for me.
#'
#' This function will not download \code{FPSIM.rds}. The \code{download_extdata()}
#' function in \pkg{peacesciencer} will do that.
#'
#' @param confirm logical, defaults to FALSE. If FALSE, the function
#' does not actually download the data. Set this to TRUE to confirm your
#' intentions to download the data.
#' @param format a character vector determining what format to download. Defaults
#' to "rds" for the R serialized data frame format native to R. "qs" downloads the
#' data in a \code{.qs} file for more file compression, though reading it will
#' depend on the \pkg{qs2} package.
#' @param warning logical, defaults to TRUE. If TRUE, the function returns a
#' message advising you about the total size of the files you'll be downloading.
#' If FALSE, no message is returned about the total file sizes.
#'
#' @examples
#'
#' download.fpsim.data()
#'
#' # If you use this function, please inspect the extdata/ directory of the R
#' # package on your system. This function's output will tell you where it is.
#' # Thereafter, you can manually read a particular file into R. Something like
#' # this would work, though, if you had no idea where anything is.
#' #
#' # a <- paste0(system.file("extdata", package = "fpsim"),"/fpsim-votes-a.rds")
#' # Data <- readRDS(a)
#' # Data
#'
#' @importFrom utils download.file
#' @export

download.fpsim.data <- function(confirm = FALSE, warning = TRUE, format = "rds") {


  if(warning == TRUE) {

    warn <- paste0(
      "Please be advised the sum total of external data will take up about 70 total MB of disk space. ",
      "Documentation on the package's website (svmiller.com/fpsim) will advise you how large these individual files are for download. ",
      "Please consult the files available on the website in case data updates outpace the update of this R package. ",
      "You must set warning = FALSE to disable this message. This is more a point of information for you, the user.\n"
    )
    message(warn)
  }

  if(confirm == FALSE) {

    conff <- paste0(
      "\nCRAN generally frowns on functions that foist non-interactive execution on the user or assume a console session. ",
      "Please set confirm = TRUE to actually get the data, which will be available to you in the package's extdata/ directory. Here:\n\n",
      system.file("extdata", package = "fpsim"),
      "\n\nThis function may not be available to you if you have file permission issues related to where your R packages install. This message should also be CRAN-compliant.")

    message(conff)

  } else { # confirm == TRUE...

    filesfordl <- c("fpsim-alliances-a-unweighted", "fpsim-alliances-a-weighted",
                    "fpsim-alliances-pk", "fpsim-alliances-s-unweighted",
                    "fpsim-alliances-s-weighted", "fpsim-alliances-taub",
                    "fpsim-votes-a", "fpsim-votes-pk")

    if(!format %in% c("qs", "rds")) {

      stop("format must be one of 'qs' or 'rds'")
    } else {

      filesfordl <- paste0(filesfordl, paste0(".",format))
    }

    dest <- system.file("extdata", package = "fpsim")


    weburl <- "https://svmiller.com/fpsim/data/"
    fileurls <- paste0(weburl, filesfordl)

    mapply(
      download.file,
      url = fileurls,
      destfile = file.path(dest, filesfordl),
      MoreArgs = list(mode = "wb")
    )

    message(paste0("\n\nCheck the contents of the /extdata directory below:\n\n",
                   dest))

  }

}
