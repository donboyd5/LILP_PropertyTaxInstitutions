


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library("scales")
library("hms") # hms, for times.
library("lubridate") # lubridate, for date/times.
library("readxl") # readxl, for .xls and .xlsx files.
library("haven") # haven, for SPSS, SAS and Stata files.
library("vctrs")
library("precis")

library("tibbletime") # https://business-science.github.io/tibbletime/

library("btools") # library that I created (install from github)
library("bdata")

library("qtax")

library("BEAData")

#****************************************************************************************************
#                functions ####
#****************************************************************************************************



#****************************************************************************************************
#                prepare initial-year data ####
#****************************************************************************************************
#.. initial assessment roll ----
# aroll0 %>%
#   group_by(ptype) %>%
#   summarise_at(vars(mv0, av0, tav0, rev0), funs(sum)) %>%
#   mutate(mvshare=mv0 / sum(mv0) * 100,
#          avshare=av0 / sum(av0) * 100,
#          avratio=av0 / mv0 * 100,
#          tavshare=tav0 / sum(tav0) * 100,
#          revshare=rev0 / sum(rev0) * 100
#          )


# Notes:
# mv.ar0 is market value on the assessment roll at time zero

aroll0 <- read_csv("unit, ptype, mv0, mv.ar0, av0, tav0, rev0, yearpurch0
r1, res, 100e3, 100e3, 50e3, 40e3, 400, 2010
r2, res, 100e3, 100e3, 50e3, 45e3, 450, 2012
c1, com, 200e3, 200e3, 200e3, 200e3, 4e3, 2015
c2, com, 100e3, 100e3, 100e3, 100e3, 2e3, 2015
")
aroll0

#.. create aroll.base, a data frame with the structure we want, for all years we will look at ----
nyears <- 10
stub <- merge(data.frame(year=1:nyears), aroll0, by=NULL)
stub

aroll.base <- stub %>%
  mutate_at(vars(mv0, mv.ar0, av0, tav0, rev0), funs(x=ifelse(year==1, ., NA))) %>%
  rename_at(vars(contains("0_x")), funs(str_remove(., "0_x")))
aroll.base


#****************************************************************************************************
#                establish assumptions ####
#****************************************************************************************************
#.. market-value growth rates by year and property type ----
grates.res <- tibble(year=1:10, mvgr=.05, ptype="res")
grates.com <- tibble(year=1:10, mvgr=.03, ptype="com")
grates <- bind_rows(grates.res, grates.com) %>%
  arrange(ptype, year) %>%
  group_by(ptype) %>%
  mutate(mvg.factor=cumprod(1 + c(0, mvgr[-1])))
grates


rules <- list()

rules$res <- list()
rules$res$mvlag <- 3
rules$res$avratio <- .6
rules$res$tav.grcap <- .02

rules$com <- list()
rules$com$mvlag <- 3
rules$com$avratio <- .8
rules$com$tav.grcap <- Inf
rules

rules.df <- rules %>%
  as.data.frame %>%
  gather(variable, value) %>%
  separate(variable, c("ptype", "rule"), extra="merge") %>%
  spread(rule, value)
rules.df


#****************************************************************************************************
#                enhance the data frame with assumptions ####
#****************************************************************************************************

aroll.base2 <- aroll.base %>%
  left_join(grates) %>%
  left_join(rules.df) %>%
  group_by(ptype, unit) %>%
  arrange(year) %>%
  mutate(mv=mv[1] * mvg.factor) %>%
  ungroup %>%
  arrange(ptype, unit, year)
aroll.base2


# now we are ready to walk through time
upvals <- function(df, nyears, grate){
  for(y in 2:nyears){
    df$mv[y] <- df$mv[y - 1] * (1 + grate)
  }
  return(df)
}

aroll <- aroll.base %>%
  group_by(ptype, unit) %>%
  arrange(year) %>%
  do(upvals(., 10, .05))
aroll


#****************************************************************************************************
#                run the model ####
#****************************************************************************************************
aroll.base2

get_mv.ar <- function(mv, mvgr, mvlag){
  # compute pre-year-1 values of mv, based on growth rate and # of lags
  mv0 <- mv[1]
  mvgr0 <- mvgr[1]
  mvlag0 <- mvlag[1]
  mvlagged <- lag(mv, mvlag0)
  mvprior <-  mv0 / (1 + mvgr0) ^ (mvlag0 - 1:length(mv) + 1)
  mv.ar <- c(mvprior[1:mvlag0], mvlagged[(mvlag0 + 1):length(mvlagged)])
  return(mv.ar)
}

aroll <- aroll.base2 %>%
  group_by(ptype, unit) %>%
  mutate(mv.ar=get_mv.ar(mv, mvgr, mvlag))
aroll
         