# Calculate Kendall's (1938) Tau-B

`taub()` takes two vectors and returns Kendall's Tau-b as a measure of
rank correlation.

## Usage

``` r
taub(x1, x2)
```

## Arguments

- x1:

  a vector, and one assumes an integer

- x2:

  a vector, and one assumes an integer

## Value

`taub()` takes two vectors and returns Kendall's Tau-b as a measure of
rank correlation.

## Details

I'll be honest that I wrote this just to say that I did write this and
that my workflow would still lean on using the
[`cor()`](https://rdrr.io/r/stats/cor.html) function in base R.

## References

Kendall, Maurice G. 1938. "A New Measure of Rank Correlation".
*Biometrika* 30(1/2): 81-93.

## Examples

``` r

taub(usamex46$vote1, usamex46$vote2)
#> [1] 0.01431261
taub(gmyrus14$gmy, gmyrus14$rus)
#> [1] 0.03031695

# Compare with...

cor(usamex46$vote1, usamex46$vote2, method = 'kendall')
#> [1] 0.01431261
cor(gmyrus14$gmy, gmyrus14$rus, method = 'kendall')
#> [1] 0.03031695
```
