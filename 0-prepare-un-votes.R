library(tidyverse)
library(peacesciencer)
library(isard)
library(qs2)

completeVotes %>%
  as_tibble() %>%
  filter(!is.na(ccode) & !is.na(member)) %>%
  mutate(vote = ifelse(vote %in% c(1:3), vote, NA)) %>%
  select(resid, ccode, date, year, vote) %>%
  na.omit -> UNVD


UNVD

create_statedays() -> cw_statedays

cw_statedays %>%
  filter(date >= ymd(19460101)) -> cw_statedays

# UN votes from non-system members.
anti_join(UNVD, cw_statedays,
          by = c("ccode", "date")) %>%
  summarize(first_date = first(date),
            last_date = last(date),
            n = n(),
            .by = ccode) %>%
  arrange(ccode) -> un_vote_irregularities

un_vote_irregularities
isard::cw_system %>% filter(ccode == 260)

checkem <- function(code)  {
  left_join(un_vote_irregularities, isard::cw_system) %>%
    filter(ccode == code)
}

checkem(260)

# A few things here:
# - West German (260) dates coincide with its exit from the CoW system and
#   unification with East Germany. A similar thing happened with Yemen. There
#   are no 255 votes in 1990 despite unification happening effective Oct. 3, 1990.
#   There are incidentally no East German votes either in 1990 and anything
#   attributed to them is curiously after unification (which is certainly odd).
#   Given the chain of events, I'll attribute these to unified Germany.
# - Ukraine (369) was conspicuous for a kind of favored child of the USSR and having
#    a seat in the United Nations. Keep the 1991 observations for calculating
#    FPSIM for the year in which it did become independent, but remove everything else.
# - Same for Belarus (370), though admittedly didn't know as much about this case.
# - Guinnea-Bissau (404) appears in the system 8 days after these two votes. Keep?
# - Niger (436) appears in the system two days after these votes. Keep.
# - Keep Uganda (500), same reason as above
# - Keep Zanzibar (511), Comoros (581).
# - I was surprised that any issues re: UAR did not appear here. Syria joins the
#   system on April 17, 1946 and these 17 observations are from earlier that
#   year. Keep.
# - Keep Lebanon (660) for reason above.
# - The Yemeni cases are interesting. The observations for YAR are after
#   unification. YAR and YPR end on May 21, 1990 and unified Yemen starts on
#   May 22. There are no invalid YPR votes afterward. All 679 votes are faithfully
#   recorded in 1991. I'm going to treat any votes on/after May 22, 1990 as votes
#   of the new unified Yemen.
# - Keep Qatar (694)
# - Oh lord, this case. Okay, I think I see what happened in the data. There are
#   249 Taiwanese (713) votes from Jan. 1, 1946 to Dec. 3, 1949. Taiwan enters
#   the system on Dec. 8, 1949. Some context clues here suggest the data are
#   using state codes to describe a government and not a state. The first actual
#   China (PRC) votes don't appear until 1971. Who sits in the "China" seat was
#   a big to-do. From the start of the data until Nov. 15, 1971, this was ROC.
#   But ROC wasn't "China" after the retreat of the nationalist government on
#   Dec. 7, 1949. So, here's the basic issue: any UN votes before Dec. 7, 1949
#   belong to ccode 710. Anything between Dec. 8, 1949 and Oct. 25, 1971 belongs
#   to 713. That was UN General Assembly Resolution 2758 (XXVI). PRC begins
#   representing "China" on Nov. 15, 1971. There are actually a series of
#   observations in 1971 before this date, but I will remove them in case there
#   is contamination.
# - Remove the Indian (750) observations from 1946.
# - Keep the Bhutan (760) observations from 1971.
# - Keep the Philippines (840) observations from 1946.
# - Keep the Vanuatu observations (935). Same for Fiji in 1970.

# Okay, you got all that?

UNVD %>%
  mutate(ccode = ifelse(ccode == 260 & year == 1990, 255, ccode)) %>%
  filter(!(ccode == 369 & year <= 1990)) %>%
  filter(!(ccode == 370 & year <= 1990)) %>%
  mutate(ccode = ifelse(ccode == 678 & year == 1990, 679, ccode)) %>%
  mutate(ccode = case_when(
    ccode == 713 & date <= ymd(19491207) ~ 710,
    ccode == 710 & between(date, ymd(19710101), ymd(19711114)) ~ NA_real_,
    TRUE ~ ccode
  )) %>%
  filter(!is.na(ccode)) -> UNVD

UNVD %>%
  group_split(year) -> splitYear

names(splitYear) <- unique(UNVD$year)

for (i in names(splitYear)) {
  qs_save(splitYear[[i]], paste0("data-raw/un/y",i, '.qs'))
}
