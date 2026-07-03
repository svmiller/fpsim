library(tidyverse)
library(fpsim)

# This is a note to self to compare how I initially wrote srs() (based on my
# own individual reconstructions of this measure) to better conform to how I
# wrote spi() and cohenk(). It features in how I wrote bcai(), which itself
# requires the construction of S. It comes from me reading Benati and Capurri
# (2026) to reconstruct their A index, which is basically a chance-corrected
# S that subtracts from S what could be expected under the assumption of jointly
# independent voting/ranking.

# First, let's show the output of srs() and with the example data to reproduce
# what Signorino and Ritter (1999) show in their Table 6.

srs(gmyrus14$gmy, gmyrus14$rus, distances = "absolute")


# Now, let's do it another way. I'll be borrowing from the functions I wrote,
# which leans on an x1, x2, and a specified leve.s

x1 <- gmyrus14$gmy
x2 <- gmyrus14$rus

use.these.levels <- c(0:3)

completetf <- complete.cases(x1, x2)

x1 <- x1[completetf]
x2 <- x2[completetf]

# Calculate unweighted, absolute values S first...
tab <- table(factor(x1, levels = use.these.levels),
             factor(x2, levels = use.these.levels))

o <- prop.table(tab)

rmarg <- rowSums(o)
cmarg <- colSums(o)

d <- abs(row(o) - col(o))
dd <- nrow(o) - 1

S <- 1 - 2*sum(o*d)/dd
S


# Now, let's peep the weights. First, reproducing Table 6's weighted S score.

srs(gmyrus14$gmy, gmyrus14$rus, distances = 'absolute', weights = gmyrus14$syscap)


# Then, do it manually. I'll just have to be mindful that cases are complete.
# They are here, but I can't assume that in the functions I write...
weights <- gmyrus14$syscap

o <- xtabs(weights ~
             factor(x1, levels = levs) +
             factor(x2, levels = levs))

rmarg <- rowSums(o)
cmarg <- colSums(o)


d  <- abs(row(o) - col(o))
dd <- nrow(o) - 1

S <- 1 - 2 * sum(o * d) / dd
S

# Not bad. I probably should've done it this way earlier.
