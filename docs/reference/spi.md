# Calculate Scott's (1955) pi

`spi()` takes two vectors and returns Scott's (1955) pi coefficient,
communicating extent of inter-observer reliability.

## Usage

``` r
spi(x1, x2, levels = NULL)
```

## Arguments

- x1:

  a vector, and one assumes an integer

- x2:

  a vector, and one assumes an integer

- levels:

  defaults to NULL, but an optional vector that defines the full
  sequence of values that could be observed in `x1` and `x2`. If NULL,
  the function looks for observed values.

## Value

`spi()` takes two vectors and returns Scott's (1955) pi coefficient,
communicating extent of inter-observer reliability.

## Details

The function subsets to complete cases of the two vectors for which you
want Scott's pi.

The function implicitly assumes that `x1` and `x2` are columns in a data
frame. One indirect check for this looks at whether `x1` and `x2` are
the same length. The function will stop if they're not.

There will sometimes be instances, assuredly with alliances, where not
all categories are observed. For example, the toy example I provide of
Germany and Russia in 1914 includes no 2s. In the language of "ratings",
the "rating" of 2 was available for Germany and Russia in 1914 but
neither side used it. The `levels` argument allows you to specify the
full sequence of values that could be observed, even if none were. It
probably makes the most sense to always use this argument, even if the
default behavior operates as if you won't.

## References

Scott, William A. 1955. "Reliability of Content Analysis: The Case of
Nominal Scale Coding." *Public Opinion Quarterly* 19(3): 321–5.

## Examples

``` r

spi(gmyrus14$gmy, gmyrus14$rus, levels = 0:3) # with levels argument
#> [1] -0.04477612
spi(usamex46$vote1, usamex46$vote2) # levels argument not necessary here.
#> [1] -0.02775801
```
