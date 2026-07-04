# Calculate Benati and Capurri's (2026) alignment index (A)

`bcai()` takes two vectors and returns Benati and Capurri's (2026)
alignment index (A).

## Usage

``` r
bcai(x1, x2, distances = "absolute", weights = NULL, levels = NULL)
```

## Arguments

- x1:

  a vector, and one assumes an integer

- x2:

  a vector, and one assumes an integer

- distances:

  the type of distances between ratings/attachments to estimate. Can be
  either "absolute" or "squared". Defaults to "absolute", but see note
  in details section.

- weights:

  a vector of weights. Defaults to NULL for creating unweighted A index
  values.

- levels:

  defaults to NULL, but an optional vector that defines the full
  sequence of values that could be observed in `x1` and `x2`. If NULL,
  the function looks for observed values.

## Value

`bcai()` takes two vectors and returns Benati and Capurri's (2026)
alignment index (A).

## Details

You can think of the alignment index that Benati and Capurri (2026)
describe as an *S* corollary to the chance-corrected measures that Häge
(2011) offers as substitutes for *S*. It takes the (unweighted, absolute
distances) *S* score proposed by Signorino and Ritter (1999) and
subtracts from it the *S* score that would follow under the assumption
of independent voting.

The function subsets to complete cases of the two vectors for which you
want an alignment score.

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

### A Few Caveats on Weighting

You can weight this measure if you want. Please be mindful about what
you're doing, especially if the weights are CINC scores. See here:

<https://svmiller.com/blog/2026/06/alliances-weighting-foreign-policy-similarity/>

The function will proportionalize your weights to sum to 1 if they do
not sum to 1 already.

## References

Benati, Stefano, and Agnese Capurri. 2026. "The Alignment index and its
application to voting at the United Nations General Assembly." *Quality
& Quantity*.
[doi:10.1007/s11135-026-02814-x](https://doi.org/10.1007/s11135-026-02814-x)

## Examples

``` r

bcai(gmyrus14$gmy, gmyrus14$rus, levels = 0:3) # with levels argument
#> [1] -0.04
bcai(bencapex$rowv, bencapex$colv) # levels argument not necessary here.
#> [1] -0.07291667
bcai(gmyrus14$gmy, gmyrus14$rus, distances = 'squared', levels = 0:3) # squared, with levels argument
#> [1] -0.04
```
