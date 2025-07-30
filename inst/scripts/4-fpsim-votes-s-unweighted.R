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
FPSIMVSU <- foreach(
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
    declare_attributes("dyad_year", system = 'cow') %>%
    add_nmc(keep = 'cinc') -> UNDDY

  UNDDY %>%
    split(., paste(.$ccode1, .$ccode2, sep = "_")) -> yearsplits

  yearsplits %>%
    map(~srs(.$vote1, .$vote2, range = 2, distances = 'absolute')) %>%
    enframe(., name = "dyad", value = "svotevua") %>%
    unnest(svotevua) -> VUA

  yearsplits %>%
    map(~srs(.$vote1, .$vote2, range = 2, distances = 'squared')) %>%
    enframe(., name = "dyad", value = "svotevus") %>%
    unnest(svotevus) -> VUS

  # yearsplits %>%
  #   map(~srs(.$vote1, .$vote2, range = 2, distances = 'absolute')) %>%
  #   enframe(., name = "dyad", value = "svotevwa") %>%
  #   unnest(svotevua) -> VWA

  full_join(VUA, VUS, by=c("dyad" = "dyad")) %>%
    separate(dyad, c("ccode1", "ccode2")) %>%
    mutate(ccode1 = as.numeric(ccode1),
           ccode2 = as.numeric(ccode2)) -> here_it_is


  print(paste("Ending", y, "on", Sys.time()))
  # ^ definitely don't end with this... Steve... okay...

  here_it_is

}

toc(log = TRUE) # and, time
parallel::stopCluster(cl = my.cluster) # close our clusters
rm(my.cluster)


qs_save(FPSIMVSU, "docs/data/FPSIMVSU.qs")


sink(file = "inst/scripts/4-fpsim-votes-s-unweighted.log")
timestamp()
tic.log()
sink()
