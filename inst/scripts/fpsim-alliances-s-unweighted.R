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
FPSIMASU <- foreach(
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
  svusmatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))
  # s ([v]alued, [u]nweighted, [a]bsolute)
  svuamatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))
  # s ([b]inary, [u]nweighted, [s]quared)
  sbusmatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))
  # s ([b]inary, [u]nweighted, [a]bsolute)
  sbuamatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))


  for (i in 1:n) {
    for (j in i:n) {

      ##------ Signorino and Ritter's (1999) s ------##
      # s ([v]alued, [u]nweighted, [s]quared)
      #svusscores <- srs(V[i, ], V[j, ], data = "alliances", distances = "squared", ordered = TRUE)
      #svuascores <- srs(V[i, ], V[j, ], data = "alliances", distances = "absolute", ordered = TRUE)
      #sbusscores <- srs(B[i, ], B[j, ], data = "alliances", distances = "squared", ordered = FALSE)
      #sbuascores <- srs(B[i, ], B[j, ], data = "alliances", distances = "absolute", ordered = FALSE)

      svusscores <- srs(t(V[i, ]), t(V[j, ]), distances = 'squared', levels = 0:3)
      svuascores <- srs(t(V[i, ]), t(V[j, ]), distances = 'absolute', levels = 0:3)
      sbusscores <- srs(t(B[i, ]), t(B[j, ]), distances = 'squared', levels = 0:1)
      sbuascores <- srs(t(B[i, ]), t(B[j, ]), distances = 'absolute', levels = 0:1)



      ##########################################
      ##------ Now fill in the matrices ------##
      ##########################################

      # The s scores...
      svusmatrix[i, j] <- svusscores
      svusmatrix[j, i] <- svusscores  # symmetric

      svuamatrix[i, j] <- svuascores
      svuamatrix[j, i] <- svuascores  # symmetric

      sbusmatrix[i, j] <- sbusscores
      sbusmatrix[j, i] <- sbusscores  # symmetric

      sbuamatrix[i, j] <- sbuascores
      sbuamatrix[j, i] <- sbuascores  # symmetric

    }
  }

  svusmatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, sallyvus, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    mutate(year = y) %>%
    select(ccode1, ccode2, year, everything()) -> here_it_is

  svuamatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, sallyvua, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    left_join(here_it_is, .,
              by = c("ccode1" = "ccode1",
                     "ccode2" = "ccode2")) -> here_it_is

  sbusmatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, sallybus, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    left_join(here_it_is, .,
              by = c("ccode1" = "ccode1",
                     "ccode2" = "ccode2")) -> here_it_is

  sbuamatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, sallybua, -ccode1) %>%
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

FPSIMASU %>%
  bind_rows() %>%
  filter(ccode1 != ccode2) -> FPSIMASU

qs_save(FPSIMASU, "docs/data/fpsim-alliances-s-unweighted.qs")
saveRDS(FPSIMASU, "docs/data/fpsim-alliances-s-unweighted.rds")

sink(file = "inst/scripts/fpsim-alliances-s-unweighted.log")
timestamp()
tic.log()
sink()
