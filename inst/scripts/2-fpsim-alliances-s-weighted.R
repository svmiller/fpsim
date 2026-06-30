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
# CINC now exceeds the ATOP right bound.

ATOPDDY <- qs_read("data-raw/atop/ATOPDDY.qs")


tic()
FPSIMASW <- foreach(
  y = startyear:endyear
) %dopar% {

  print(paste("Starting", y, "on", Sys.time()))

  ATOPDDY %>%
    filter(year == y) %>%
    distinct(ccode2, cinc) %>%
    pull(cinc) -> cincweights

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


  ## s ([v]alued, [w]eighted, [s]quared)
  #svwsmatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))
  # s ([v]alued, [w]eighted, [a]bsolute)
  svwamatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))
  ## s ([b]inary, [w]eighted, [s]quared)
  # sbwsmatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))
  # s ([b]inary, [w]eighted, [a]bsolute)
  sbwamatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))


  for (i in 1:n) {
    for (j in i:n) {

      ##------ Signorino and Ritter's (1999) s ------##
      # s ([v]alued, [u]nweighted, [s]quared)
      #svusscores <- srs(V[i, ], V[j, ], data = "alliances", distances = "squared", ordered = TRUE)
      #svuascores <- srs(V[i, ], V[j, ], data = "alliances", distances = "absolute", ordered = TRUE)
      #sbusscores <- srs(B[i, ], B[j, ], data = "alliances", distances = "squared", ordered = FALSE)
      #sbuascores <- srs(B[i, ], B[j, ], data = "alliances", distances = "absolute", ordered = FALSE)

      #svwsscores <- srs(t(V[i, ]), t(V[j, ]), distances = 'squared', range = 3, weights = cincweights)
      svwascores <- srs(t(V[i, ]), t(V[j, ]), distances = 'absolute', range = 3, weights = cincweights)
      #sbwsscores <- srs(t(B[i, ]), t(B[j, ]), distances = 'squared', range = 1, weights = cincweights)
      sbwascores <- srs(t(B[i, ]), t(B[j, ]), distances = 'absolute', range = 1, weights = cincweights)



      ##########################################
      ##------ Now fill in the matrices ------##
      ##########################################

      # The s scores...
      # svwsmatrix[i, j] <- svwsscores
      # svwsmatrix[j, i] <- svwsscores  # symmetric

      svwamatrix[i, j] <- svwascores
      svwamatrix[j, i] <- svwascores  # symmetric

      # sbwsmatrix[i, j] <- sbwsscores
      # sbwsmatrix[j, i] <- sbwsscores  # symmetric

      sbwamatrix[i, j] <- sbwascores
      sbwamatrix[j, i] <- sbwascores  # symmetric

    }
  }

  svwamatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, sallyvwa, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    mutate(year = y) %>%
    select(ccode1, ccode2, year, everything()) -> here_it_is

  # svwamatrix %>% as_tibble() %>%
  #   mutate(ccode1 = ccodes) %>%
  #   gather(ccode2, sallyvwa, -ccode1) %>%
  #   arrange(ccode1) %>%
  #   mutate(ccode2 = as.numeric(ccode2)) %>%
  #   left_join(here_it_is, .,
  #             by = c("ccode1" = "ccode1",
  #                    "ccode2" = "ccode2")) -> here_it_is
  #
  # sbwsmatrix %>% as_tibble() %>%
  #   mutate(ccode1 = ccodes) %>%
  #   gather(ccode2, sallybws, -ccode1) %>%
  #   arrange(ccode1) %>%
  #   mutate(ccode2 = as.numeric(ccode2)) %>%
  #   left_join(here_it_is, .,
  #             by = c("ccode1" = "ccode1",
  #                    "ccode2" = "ccode2")) -> here_it_is

  sbwamatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, sallybwa, -ccode1) %>%
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


qs_save(FPSIMASW, "docs/data/FPSIMASW.qs")
saveRDS(FPSIMASW, "docs/data/FPSIMASW.rds")

sink(file = "inst/scripts/2-fpsim-alliances-s-weighted.log")
timestamp()
tic.log()
sink()
