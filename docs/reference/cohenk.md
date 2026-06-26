# Calculate Cohen's (1960, 1968) weighted kappa

`cohenk()` takes two vectors and returns Cohen's kappa as an estimate of
chance-corrected agreement.

## Usage

``` r
cohenk(x1, x2, w_exp = 2, levels = NULL)
```

## Arguments

- x1:

  a vector, and one assumes an integer

- x2:

  a vector, and one assumes an integer

- w_exp:

  an exponent to apply to the weight matrix. Default is 2 for squared
  distances in the weight matrix. Supplying a 1 would make for linear
  distances.

- levels:

  defaults to NULL, but an optional vector that defines the full
  sequence of values that could be observed in `x1` and `x2`. If NULL,
  the function looks for observed values.

## Value

`cohenk()` takes two vectors and returns Cohen's kappa as an estimate of
chance-corrected agreement.

## Details

The function subsets to complete cases of the two vectors for which you
want Cohen's kappa.

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

Cohen, Jacob. 1960. "A Coefficient of Agreement for Nominal Scales."
*Educational and Psychological Measurement* 20(1): 37-46.

Cohen, Jacob. 1968. "Weighted Kappa: Nominal Scale Agreement with
Provision for Scaled Disagreement or Partial Credit." *Psychological
Bulletin* 70(4): 213–220.

## Examples

``` r

cohenk(gmyrus14$gmy, gmyrus14$rus, levels = 0:3) # with levels argument
#> [1] -0.07758621
cohenk(usamex46$vote1, usamex46$vote2) # levels argument not necessary here.
#> [1] -0.01222707
```
