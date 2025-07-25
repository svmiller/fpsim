library(tidyverse)
library(peacesciencer)
library(isard)
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

srs <- function(x, y, data = "alliances",
                distances = "squared", ordered = FALSE) {

  if(data == "alliances") {
    if(distances == "squared") {

      num <-  sum((x - y)^2)

      if(ordered == TRUE) {
        denom <- length(x)*(3^2)
      } else {
        denom <- length(x)*(1^2)
      }
      1 - 2*(num/denom)

    } else if(distances == "absolute") {

      num <- sum(abs(x - y))

      if(ordered == TRUE) {
        denom <- length(x)*3
      } else {
        denom <- length(x)
      }
      x <- 1 - 2*(num/denom)

    }
  } else if(data == "voting") {
    # Note: I'll keep the ability to do simple matched votes rather than
    # valued votes, but I won't use it.

    if(distances == "squared") {

      num <-  sum((x - y)^2)

      if(ordered == TRUE) {
        denom <- length(x)*(2^2)
      } else {
        denom <- length(x)*(1^2)
      }
      1 - 2*(num/denom)

    } else if(distances == "absolute") {

      num <- sum(abs(x - y))

      if(ordered == TRUE) {
        denom <- length(x)*2
      } else {
        denom <- length(x)
      }
      x <- 1 - 2*(num/denom)

    }


  }


}

create_dyadyears() -> DDY
attr(DDY, "ps_system") <- NULL
attr(DDY, "ps_data_type") <- NULL

sATOP <- haven::read_dta("/home/steve/Koofr/data/atop/5.1/atop-sscore.dta")
ATOP <- haven::read_dta('/home/steve/Koofr/data/atop/5.1/atop5_1ddyr.dta')
# ^ just as an FYI, Steve, there's right bound here at 2018.

ATOP %>%
  mutate(ccode2 = str_sub(ddyad, -3, -1),
         ccode1 = str_sub(ddyad, 1, -4)) %>%
  select(ddyad, ccode1, ccode2, year, everything()) %>%
  mutate(ccode1 = as.numeric(ccode1),
         ccode2 = as.numeric(ccode2)) %>%
  select(ccode1:consul) %>%
  left_join(DDY %>% filter(year <= 2018), .) %>%
  mutate(across(atopally:consul, ~ifelse(is.na(.), 0, .))) -> DDY


# Okay, here comes the stuff that I hate, though I understand that this is the
# convention. You should not think of alliances as "valued" in any way, shape,
# or form. No matter, this is the way it's done and been done. We all have our
# little things in the profession that we don't like, and I don't doubt for a
# second I have things that I do/think are right that others would have a similar
# aversion too. It's all gravy. The extent to which we continue doing this,
# Chiba et al. (2022) suggest the following ranking based on the ATOP data.
#
# 3 = defense and/or offense (regardless of other content).
# 2 = neutrality and/or consultation (but no defense or offense obligations)
# 1 = nonaggression (but no defense, offense, neutrality, or consultation)
# 0 = no alliance obligation

DDY %>%
  distinct(atopally, defense, offense, neutral, nonagg, consul) -> proof_o_concept


proof_o_concept %>%
  mutate(ordatop = case_when(
    atopally == 0 ~ 0,
    defense == 1 | offense == 1 ~ 3,
    (neutral == 1 | consul == 1) & (defense == 0 & offense == 0) ~ 2,
    nonagg  == 1 & (defense == 0 & offense == 0 & neutral == 0 & consul == 0) ~ 1,
    TRUE ~ 0 # this will capture those asymm observations where there is an alliance but no obligation
  )) %>% data.frame

# Hmm, is that a 'shareob' that I just wanted to pass over/ignore.

ATOP %>%
  filter(nonagg == 0 & defense == 0 & offense == 0 & neutral == 0 & consul == 0) %>%
  summary

# No, but they are all asymm. Interesting. If we follow the letter of the codebook
# provided by the authors, this is an absence of an "obligation", so they should
# be 0.

DDY %>%
  mutate(ordatop = case_when(
    atopally == 0 ~ 0,
    defense == 1 | offense == 1 ~ 3,
    (neutral == 1 | consul == 1) & (defense == 0 & offense == 0) ~ 2,
    nonagg  == 1 & (defense == 0 & offense == 0 & neutral == 0 & consul == 0) ~ 1,
    TRUE ~ 0 # this will capture those asymm observations where there is an alliance but no obligation
  )) -> DDYV

DDYV %>%
  expand(ccode1 = ccode1, ccode2 = ccode2, year = year,
         ordatop = 3) %>%
  filter(ccode1 == ccode2) %>%
  left_join(., state_panel() %>%
              filter(year <= 2018) %>%
              as_tibble() %>%
              mutate(in_system = 1) %>%
              rename(ccode1 = ccode) %>%
              select(ccode1, year, in_system)) %>%
  filter(!is.na(in_system)) %>%
  select(-in_system) %>%
  bind_rows(DDYV, .) %>%
  arrange(ccode1, year,  ccode2) -> DDYV

DDYV %>%
  mutate(binatop = ifelse(ordatop >= 1, 1, 0)) -> DDYV



tic()
FPSIMALLY <- foreach(
  y = startyear:endyear
) %dopar% {

  print(paste("Starting", y, "on", Sys.time()))
  # Split into valued (V) and binary (B)
  DDYV %>%
    filter(year == y) %>%
    select(ccode1, ccode2, ordatop) %>%
    spread(ccode2, ordatop) -> V

  DDYV %>%
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
  # Scott's pi ([v]alued)
  pvmatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))
  # Scott's pi ([b]inary)
  pbmatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))
  # Cohen's kappa ([v]alued)
  kvmatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))
  # Cohen's kappa ([b]inary)
  kbmatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))
  # tau-b (valued, not that you should use it)
  taubmatrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(ccodes, ccodes))


  for (i in 1:n) {
    for (j in i:n) {

      ##------ Signorino and Ritter's (1999) s ------##
      # s ([v]alued, [u]nweighted, [s]quared)
      svusscores <- srs(V[i, ], V[j, ], data = "alliances", distances = "squared", ordered = TRUE)
      svuascores <- srs(V[i, ], V[j, ], data = "alliances", distances = "absolute", ordered = TRUE)
      sbusscores <- srs(B[i, ], B[j, ], data = "alliances", distances = "squared", ordered = FALSE)
      sbuascores <- srs(B[i, ], B[j, ], data = "alliances", distances = "absolute", ordered = FALSE)

      ##------ Scott's (1955) pi ------##
      vvv <- as.data.frame(t(rbind(V[i,], V[j,])))
      bbb <- as.data.frame(t(rbind(B[i,], B[j,])))

      pov <- sum(vvv$V1 == vvv$V2)/nrow(vvv)
      pob <- sum(bbb$V1 == bbb$V2)/nrow(bbb)

      allratsv <- c(vvv$V1, vvv$V2)
      rattabv <- table(allratsv)

      allratsb <- c(bbb$V1, bbb$V2)
      rattabb <- table(allratsb)


      probsv <- rattabv/(2*nrow(vvv))
      probsb <- rattabb/(2*nrow(bbb))

      pev <- sum(probsv^2)
      peb <- sum(probsb^2)

      pvscores <- (pov - pev)/(1 - pev)
      pbscores <- (pob - peb)/(1 - peb)

      ##------ Cohen's (1968) kappa ------##

      kvscores <- psych::cohen.kappa(t(rbind(V[i,], V[j,]))) %>%
        broom::tidy() %>%
        filter(type == "weighted") %>%
        pull(estimate)

      kbscores <- psych::cohen.kappa(t(rbind(B[i,], B[j,]))) %>%
        broom::tidy() %>%
        filter(type == "weighted") %>%
        pull(estimate)

      ##------ Kendall's (1938) tau-b ------##
      ttt <- as.data.frame(t(rbind(V[i,], V[j,])))
      taubscores <- cor(ttt$V1, ttt$V2, method = 'kendall')

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

      # Scott's pis...
      pvmatrix[i, j] <- pvscores
      pvmatrix[j, i] <- pvscores

      pbmatrix[i, j] <- pbscores
      pbmatrix[j, i] <- pbscores

      # Cohen's kappas...
      kvmatrix[i, j] <- kvscores
      kvmatrix[j, i] <- kvscores

      kbmatrix[i, j] <- kbscores
      kbmatrix[j, i] <- kbscores

      # Tau-b, not that you should...
      taubmatrix[i, j] <- taubscores
      taubmatrix[j, i] <- taubscores

    }
  }

  svusmatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, sallyvs, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    mutate(year = y) %>%
    select(ccode1, ccode2, year, everything()) -> here_it_is

  svuamatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, sallyva, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    left_join(here_it_is, .,
              by = c("ccode1" = "ccode1",
                     "ccode2" = "ccode2")) -> here_it_is

  sbusmatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, sallybs, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    left_join(here_it_is, .,
              by = c("ccode1" = "ccode1",
                     "ccode2" = "ccode2")) -> here_it_is

  sbuamatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, sallyba, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    left_join(here_it_is, .,
              by = c("ccode1" = "ccode1",
                     "ccode2" = "ccode2")) -> here_it_is

  pvmatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, pallyv, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    left_join(here_it_is, .,
              by = c("ccode1" = "ccode1",
                     "ccode2" = "ccode2")) -> here_it_is

  pbmatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, pallyb, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    left_join(here_it_is, .,
              by = c("ccode1" = "ccode1",
                     "ccode2" = "ccode2")) -> here_it_is

  kvmatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, kallyv, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    left_join(here_it_is, .,
              by = c("ccode1" = "ccode1",
                     "ccode2" = "ccode2")) -> here_it_is

  kbmatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, kallyb, -ccode1) %>%
    arrange(ccode1) %>%
    mutate(ccode2 = as.numeric(ccode2)) %>%
    left_join(here_it_is, .,
              by = c("ccode1" = "ccode1",
                     "ccode2" = "ccode2")) -> here_it_is

  taubmatrix %>% as_tibble() %>%
    mutate(ccode1 = ccodes) %>%
    gather(ccode2, taub, -ccode1) %>%
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


qs_save(FPSIMALLY, "data/FPSIMALLY.qs")


sink(file = "1-fpsim-alliances.log")
timestamp()
tic.log()
sink()
