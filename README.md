# Measures of Dyadic Foreign Policy Similarity

Let me take a big, big step back and ask what kind of data should be included
as a measure of foreign policy similarity for people to use. Before answering
this, it should be pretty clear we're in agreement about the sources of the
data.

1) Alliances (ATOP, for coverage and other reasons)
2) UN voting data

Now, there are different measures of foreign policy similarity to consider:

a) Kendall's (1938) Tau-b
b) Scott's (1955) pi
c) Cohen's (1960) kappa
d) Signorino and Ritter's (1999) S
e) ideal point distances (applicable to UN voting only)

Of these measures, a few things to further belabor:

- Should alliances be "valued" or binary? I am a strong proponent of binary
  but the valued one has a use that's almost ritualized.
- For S: should you weight the S measure by capabilities or not? If you do
  (and I don't think you at all should), you lose a few years based on the
  temporal domain of the capabilities data (along with any missing data
  weirdness). You also have this weird issue where the capabilities themselves
  should already be proportional, by year, and yet the capabilities offered do
  not sum to 1.
  Also for S: should you use squared distances or absolute value distances?
  Per `{peacesciencer}` codebook: "The choice of squared versus absolute
  distances is arbitrary. Users probably do not think about the differences,
  or know about the differences. S was usually calculated with absolute
  differences in software packages, though this was never usually belabored to
  the user. Comparability with S might be an argument in favor of absolute
  distance as a default, but keep in mind that squared distances are much more
  commonly used in most other types of distance and association metrics."

All things considered, I think this means I need to create the following:

Alliances:

- Tau-b (valued, binary)
- S (val-unweigh-sq, val-unweigh-abs, bin-unweigh-sq, bin-unweigh-abs)
- Scott's pi (valued, binary)
- Cohen's kappa (valued, binary)

UN voting (all valued):

- Tau-b
- S (unweigh-sq, unweigh-abs)
- Scott's pi
- Cohen's kappa
- ideal point distance

Ideal point distance is already provided for me (thank God), but that sums to
16(!) different metrics to calculate. I'm just going to ignore the question of
weighting versus not-weighting.
