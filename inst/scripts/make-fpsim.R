library(tidyverse)
library(qs2)
library(tictoc)

tic()
FPSIMASU <- qs_read("docs/data/FPSIMASU.qs")
FPSIMASW <- qs_read("docs/data/FPSIMASW.qs")
FPSIMAPKT <- qs_read("docs/data/FPSIMAPKT.qs")
FPSIMVPK <- qs_read("docs/data/FPSIMVPK.qs")

FPSIMASU %>%
  left_join(., FPSIMASW) %>%
  left_join(., FPSIMAPKT) %>%
  full_join(., FPSIMVPK) -> FPSIM

saveRDS(FPSIM, "docs/data/FPSIM.rds")
toc(log = TRUE) # and, time

sink(file = "inst/scripts/99-fpsim.log")
timestamp()
tic.log()
sink()
