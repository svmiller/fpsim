library(tidyverse)
library(fpsim)
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

# setting this up here for debugging purposes
startyear = 1816; endyear = 2018

ATOPDDY <- qs_read("data-raw/atop/ATOPDDY.qs")


tic()
FPSIMAAU <- foreach(
  y = startyear:endyear
) %dopar% {

  print(paste("Starting", y, "on", Sys.time()))
  # Split into valued (V) and binary (B)
  ATOPDDY %>%
    filter(year == y) %>%
    select(ccode1, ccode2, ordatop) %>%
    spread(ccode2, ordatop) -> V

  ATOPDDY %>%
    filter(year == y) %>%
    select(ccode1, ccode2, binatop) %>%
    spread(ccode2, binatop) -> B

  # for each year, grab the ccodes and nobs. nrow(V) should equal nrow(B).
  ccodes <- V$ccode1
  n <- nrow(V)

  # get rid of ccodes
  V$ccode1 <- NULL
  B$ccode1 <- NULL


  # s ([v]alued, [u]nweighted, [s]quared)
  avusmatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))
  # s ([v]alued, [u]nweighted, [a]bsolute)
  avuamatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))
  # s ([b]inary, [u]nweighted, [s]quared)
  abusmatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))
  # s ([b]inary, [u]nweighted, [a]bsolute)
  abuamatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))


  for (i in 1:n) {
    for (j in i:n) {

      ##------ Benati and Capurri's (2026) A ------##

      avusscores <- bcai(t(V[i, ]), t(V[j, ]), distances = 'squared', levels = c(0:3))
      avuascores <- bcai(t(V[i, ]), t(V[j, ]), distances = 'absolute', levels = c(0:3))
      abusscores <- bcai(t(B[i, ]), t(B[j, ]), distances = 'squared', levels = c(0:1))
      abuascores <- bcai(t(B[i, ]), t(B[j, ]), distances = 'absolute', levels = c(0:1))



      ##########################################
      ##------ Now fill in the matrices ------##
      ##########################################

      # The A scores...
      avusmatrix[i, j] <- avusscores
      avusmatrix[j, i] <- avusscores  # symmetric

      avuamatrix[i, j] <- avuascores
      avuamatrix[j, i] <- avuascores  # symmetric

      abusmatrix[i, j] <- abusscores
      abusmatrix[j, i] <- abusscores  # symmetric

      abuamatrix[i, j] <- abuascores
      abuamatrix[j, i] <- abuascores  # symmetric

    }
  }

  avusmatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, aallyvus, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    mutate(year = y) %>%
    select(ccode1, ccode2, year, everything()) -> here_it_is

  avuamatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, aallyvua, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    left_join(here_it_is, .,
              by = c("ccode1" = "ccode1",
                     "ccode2" = "ccode2")) -> here_it_is

  abusmatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, aallybus, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    left_join(here_it_is, .,
              by = c("ccode1" = "ccode1",
                     "ccode2" = "ccode2")) -> here_it_is

  abuamatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, aallybua, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    left_join(here_it_is, .,
              by = c("ccode1" = "ccode1",
                     "ccode2" = "ccode2")) -> here_it_is

  print(paste("Ending", y, "on", Sys.time()))
  # ^ definitely don't end with this... Steve... okay...

  here_it_is

}

toc(log = TRUE) # and, time
parallel::stopCluster(cl = my.cluster) # close our clusters
rm(my.cluster)

FPSIMAAU %>%
  bind_rows() %>%
  filter(ccode1 != ccode2) -> FPSIMAAU

qs_save(FPSIMAAU, "docs/data/fpsim-alliances-a-unweighted.qs")
saveRDS(FPSIMAAU, "docs/data/fpsim-alliances-a-unweighted.rds")

sink(file = "inst/scripts/fpsim-alliances-a-unweighted.log")
timestamp()
tic.log()
sink()
