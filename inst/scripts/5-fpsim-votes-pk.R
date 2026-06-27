library(tidyverse)
library(fpsim)
library(peacesciencer)
library(foreach)
library(tictoc)
library(qs2)

half_cores  <- parallel::detectCores()/2
my.cluster <- parallel::makeCluster(
  half_cores,
  type = "PSOCK"
)

doParallel::registerDoParallel(cl = half_cores)
foreach::getDoParRegistered()

tic()
FPSIMVPK <- foreach(
  y = c(1946:1963, 1965:2022)
) %dopar% {

  print(paste("Starting", y, "on", Sys.time()))

  yearlyvotes <- qs_read(paste0("data-raw/un/y",y,".qs")) %>% select(-date)

  yearlyvotes %>%
    slice(1, .by=c(resid, ccode)) %>%
    group_split(resid) %>%
    map(~expand(., resid = resid, ccode1 = ccode, ccode2 = ccode)) %>%
    map(~left_join(., yearlyvotes,
                   by = c("ccode1" = "ccode",
                          "resid" = "resid"))) %>%
    map(~rename(., vote1 = vote)) %>%
    map(~left_join(., yearlyvotes,
                   by = c("ccode2" = "ccode",
                          "resid" = "resid",
                          "year" = "year"))) %>%
    map(~rename(., vote2 = vote)) %>%
    bind_rows() %>%
    filter(ccode1 < ccode2) -> UNDDY


  UNDDY %>%
    split(., paste(.$ccode1, .$ccode2, sep = "_")) -> yearsplits

  yearsplits %>%
    map(~cohenk(.$vote1, .$vote2, levels = 1:3)) %>%
    enframe(., name = "dyad", value = "kvotev") %>%
    unnest(kvotev) -> KV

  yearsplits %>%
    map(~spi(.$vote1, .$vote2, levels = 1:3)) %>%
    enframe(., name = "dyad", value = "pvotev") %>%
    unnest(pvotev) -> PV

  yearsplits %>%
    map(~taub(.$vote1, .$vote2)) %>%
    enframe(., name = "dyad", value = "tvotev") %>%
    unnest(tvotev) -> TV

  # yearsplits %>%
  #   map(~cor(.$vote1, .$vote2, method = 'kendall'))



  full_join(KV, PV, by=c("dyad" = "dyad")) %>%
    separate(dyad, c("ccode1", "ccode2")) %>%
    mutate(ccode1 = as.numeric(ccode1),
           ccode2 = as.numeric(ccode2)) %>%
    mutate(year = y)  -> here_it_is


  TV %>% separate(dyad, c("ccode1", "ccode2")) %>%
    mutate(ccode1 = as.numeric(ccode1),
           ccode2 = as.numeric(ccode2)) %>%
    mutate(year = y) %>%
    left_join(here_it_is, .,
              by = c("ccode1" = "ccode1",
                     "ccode2" = "ccode2",
                     "year" = "year")) -> here_it_is

  # here_it_is %>%
  #   left_join(., TV) -> here_it_is


  print(paste("Ending", y, "on", Sys.time()))
  # ^ definitely don't end with this... Steve... okay...

  here_it_is

}

toc(log = TRUE) # and, time
parallel::stopCluster(cl = my.cluster) # close our clusters
rm(my.cluster)


qs_save(FPSIMVPK, "docs/data/FPSIMVPK.qs")


sink(file = "inst/scripts/5-fpsim-votes-pk.log")
timestamp()
tic.log()
sink()
