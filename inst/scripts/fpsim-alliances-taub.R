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
# Should've remembered this the first time around, but cinc runs only until 2016.

ATOPDDY <- qs_read("data-raw/atop/ATOPDDY.qs")


tic()
FPSIMAT <- foreach(
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

  # tau-b (valued, not that you should use it)
  taubmatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))


  for (i in 1:n) {
    for (j in i:n) {


      ##------ Kendall's (1938) tau-b ------##
      ttt <- as.data.frame(t(rbind(V[i,], V[j,])))
      taubscores <- cor(ttt$V1, ttt$V2, method = 'kendall')


      ##########################################
      ##------ Now fill in the matrices ------##
      ##########################################


      # Tau-b, not that you should...
      taubmatrix[i, j] <- taubscores
      taubmatrix[j, i] <- taubscores

    }
  }

  taubmatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, pallyv, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    mutate(year = y) %>%
    select(ccode1, ccode2, year, everything()) -> here_it_is


  print(paste("Ending", y, "on", Sys.time()))
  # ^ definitely don't end with this... Steve... okay...

  here_it_is

}

toc(log = TRUE) # and, time
parallel::stopCluster(cl = my.cluster) # close our clusters
rm(my.cluster)


FPSIMAT %>%
  bind_rows() %>%
  filter(ccode1 != ccode2) -> FPSIMAT

qs_save(FPSIMAT, "docs/data/alliances-taub.qs")
saveRDS(FPSIMAT, "docs/data/alliances-taub.rds")



sink(file = "inst/scripts/fpsim-alliances-taub.log")
timestamp()
tic.log()
sink()
