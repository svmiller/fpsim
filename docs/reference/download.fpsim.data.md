# Download FPSIM Data

`download.fpsim.data()` will download available pre-made dyadic foreign
policy similarity data and place its contents the package's `extdata/`
directory. It leverages R's `inst/` directory flexibility.

## Usage

``` r
download.fpsim.data(confirm = FALSE, warning = TRUE, format = "rds")
```

## Arguments

- confirm:

  logical, defaults to FALSE. If FALSE, the function does not actually
  download the data. Set this to TRUE to confirm your intentions to
  download the data.

- warning:

  logical, defaults to TRUE. If TRUE, the function returns a message
  advising you about the total size of the files you'll be downloading.
  If FALSE, no message is returned about the total file sizes.

- format:

  a character vector determining what format to download. Defaults to
  "rds" for the R serialized data frame format native to R. "qs"
  downloads the data in a `.qs` file for more file compression, though
  reading it will depend on the qs2 package.

## Details

This function is named in such a way to avoid a function clash with the
`download_extdata()` function in peacesciencer. It also comes from my
aversion to the use of underscores in R function. There is nothing wrong
with this convention, but the chatbots have ruined it for me.

This function will not download `FPSIM.rds`. The `download_extdata()`
function in peacesciencer will do that.

## Examples

``` r

download.fpsim.data()
#> Please be advised the sum total of external data will take up about 70 total MB of disk space. Documentation on the package's website (svmiller.com/fpsim) will advise you how large these individual files are for download. Please consult the files available on the website in case data updates outpace the update of this R package. You must set warning = FALSE to disable this message. This is more a point of information for you, the user.
#> 
#> CRAN generally frowns on functions that foist non-interactive execution on the user or assume a console session. Please set confirm = TRUE to actually get the data, which will be available to you in the package's extdata/ directory. Here:
#> 
#> /tmp/RtmpQwSYVJ/temp_libpath695bb9cc52c0/fpsim/extdata
#> 
#> This function may not be available to you if you have file permission issues related to where your R packages install. This message should also be CRAN-compliant.

# If you use this function, please inspect the extdata/ directory of the R
# package on your system. This function's output will tell you where it is.
# Thereafter, you can manually read a particular file into R. Something like
# this would work, though.
#
# a <- paste0(system.file("extdata", package = "fpsim"),"/fpsim-votes-a.rds")
# Data <- readRDS(a)
# Data
```
