library(tidyverse)
library(peacesciencer)
library(isard)

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
  mutate(binatop = ifelse(ordatop >= 1, 1, 0)) %>%
  select(ccode1:year, ordatop, binatop) -> DDYV
