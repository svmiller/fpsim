# American-Mexican Dyadic Voting Patterns in the United Nations, 1946

A simple example of voting patterns for the United States and Mexico in
the United Nations in 1946.

## Usage

``` r
usamex46
```

## Format

A data frame with 38 observations on the following 6 variables.

- `resid`:

  an identifier for a roll-call vote ID

- `ccode1`:

  the Correlates of War state code for the United States (2)

- `ccode2`:

  the Correlates of War state code for Mexico (70)

- `year`:

  a numeric constant for the year (1946)

- `vote1`:

  an integer for how the United States voted on the resolution
  identified in the `resid` column

- `vote2`:

  an integer for how Mexico voted on the resolution identified in the
  `resid` column

## Details

Data are from a June 2024 of the United Nations voting data provided by
Erik Voeten on his Dataverse for the project.

Valid vote values identified are 1 (yes), 2 (abstain), and 3 (no).

## References

Bailey, Michael A., Anton Strezhnev, and Erik Voeten. 2017. "Estimating
Dynamic State Preferences from United Nations Voting Data." *Journal of
Conflict Resolution* 61(2): 430-56.
