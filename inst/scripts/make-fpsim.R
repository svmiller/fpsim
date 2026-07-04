library(tidyverse)
library(qs2)

FPSIMASU <- qs_read("docs/data/fpsim-alliances-s-unweighted.qs")
FPSIMASW <- qs_read("docs/data/fpsim-alliances-s-weighted.qs")
FPSIMAAU <- qs_read("docs/data/fpsim-alliances-a-unweighted.qs")
FPSIMAAW <- qs_read("docs/data/fpsim-alliances-a-weighted.qs")
FPSIMAPK <- qs_read("docs/data/fpsim-alliances-pk.qs")
FPSIMAT <- qs_read("docs/data/fpsim-alliances-taub.qs")
FPSIMVA <- qs_read("docs/data/fpsim-votes-a.qs")
FPSIMVPK <- qs_read("docs/data/fpsim-votes-pk.qs")

FPSIMASU %>%
  left_join(., FPSIMASW) %>%
  left_join(., FPSIMAAU) %>%
  left_join(., FPSIMAAW) %>%
  left_join(., FPSIMAPK) %>%
  left_join(., FPSIMAT) %>%
  full_join(., FPSIMVA) %>%
  left_join(., FPSIMVPK) -> FPSIM

FPSIM %>% #names()
  select(ccode1:year, taub,
         sallyvua, sallybua, sallyvwa, sallybwa,
         aallybua, aallybwa,
         pallyb, kallyb, avoteva:pvotev) -> FPSIM


saveRDS(FPSIM, "docs/data/FPSIM.rds")

