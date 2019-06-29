


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
#                globals ####
#****************************************************************************************************
rcdir <- "./data/"
rcfn <- "RunControlPropTax(2).xlsx"


#****************************************************************************************************
#                get assumptions ####
#****************************************************************************************************

taxbases <- read_excel(paste0(rcdir, rcfn),
                       sheet = "taxbases",
                       range = cell_rows(c(3, NA)))
taxbases

rules <- read_excel(paste0(rcdir, rcfn),
                    sheet = "rules",
                    range = cell_rows(c(3, NA)))
rules

rules.long <- rules %>%
  gather(variable, value, -rulesname) %>%
  separate(variable, c("ptype", "rule"), extra="merge") %>%
  spread(rule, value)
rules.long

growthrates <- read_excel(paste0(rcdir, rcfn),
                          sheet = "growthrates",
                          range = cell_rows(c(3, NA)))
growthrates

expand_years <- function(df, nyears=25){
  # expand the growth rates so that we have one per year
  df2 <- tibble(year=1:nyears) %>%
    left_join(df %>% rename(year=mvgr.from)) %>%
    fill(growthratesname, mvgr) %>%
    select(growthratesname, year, mvgr)
  return(df2)
}

growthrates_expanded <-
  growthrates %>%
  filter(!is.na(growthratesname)) %>% # remove any blank records
  group_by(growthratesname) %>% # assumes the data will be correct - we only have one "from" group
  do(expand_years(.)) %>%
  mutate(mvg.factor=cumprod(1 + c(0, mvgr[-1])))
ht(growthrates_expanded)

# growthrates_expanded %>%
#   ggplot(aes(year, mvg.factor, colour=growthratesname)) + geom_line() + geom_point()

runs <- read_excel(paste0(rcdir, rcfn),
                    sheet = "runs",
                    range = cell_rows(c(3, NA))) %>%
  mutate(fullname=paste0(taxbasename, "-", growthratesname, "-", rulesname))
runs

runset <- 
  runs %>%
  left_join(growthrates_expanded) %>%
  left_join(rules.long) %>%
  left_join(taxbases)
glimpse(runset)

count(runset, runname, fullname)

count(runset, runname, fullname, ptype, unit)


#****************************************************************************************************
#                run the model ####
#****************************************************************************************************
get_mv.ar <- function(mv, mvgr, mvlag){
  # Get the market value as shown on the assessment roll (which may be lagged)
  # Because of lags, to get assessment-roll market values in the early years, we may need to compute
  # market values for years before year 1, using the growth rate and lages
  # compute pre-year-1 values of mv, based on growth rate and # of lags
  
  # vectors were passed in but we only need the year 1 values for some of them
  mv0 <- mv[1]
  mvgr0 <- mvgr[1]
  mvlag0 <- mvlag[1]
  
  mvlagged <- lag(mv, mvlag0)
  
  # how should we fill in past values -- based on growth rates, or static?
  #mvprior <-  mv0 / (1 + mvgr0) ^ (mvlag0 - 1:length(mv) + 1) # growth rates
  mvprior <-  rep(mv0, length(mv))  # static
  
  mv.ar <- c(mvprior[1:mvlag0], mvlagged[(mvlag0 + 1):length(mvlagged)])
  return(mv.ar)
}



get_tav <- function(av, tav.grcap){
  grcap <- tav.grcap[1]
  tav <- vector(mode="numeric", length=length(av))
  tav[1] <- av[1]
  for(y in 2:length(tav)){
    tav[y] <- pmin(av[y], av[y - 1] * (1 + grcap))
  }
  return(tav)
}


glimpse(runset)
results <- runset %>%
  group_by(runname, fullname, taxbasename, growthratesname, rulesname, ptype, unit) %>%
  arrange(year) %>%
  mutate(mv.ar=get_mv.ar(mv, mvgr, mvlag),
         av=mv.ar * avratio,
         tav=get_tav(av, tav.grcap)) %>%
  select(year, ptype, unit, mv, mv.ar, av, tav, everything()) %>%
  ungroup %>%
  arrange(ptype, unit, year)

glimpse(runset)


# aroll.base <- stub %>%
#   mutate_at(vars(mv0, mv.ar0, av0, tav0, rev0), funs(x=ifelse(year==1, ., NA))) %>%
#   rename_at(vars(contains("0_x")), funs(str_remove(., "0_x")))
# aroll.base






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

scen1 <- c(-.1, -.1, -.1, 0, rep(.05, 6))


grates.res <- tibble(year=1:nyears, mvgr=scen1, ptype="res")
grates.com <- tibble(year=1:nyears, mvgr=scen1, ptype="com")
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


# upvals <- function(df, nyears, grate){
#   for(y in 2:nyears){
#     df$mv[y] <- df$mv[y - 1] * (1 + grate)
#   }
#   return(df)
# }


#****************************************************************************************************
#                run the model ####
#****************************************************************************************************
aroll.base2

get_mv.ar <- function(mv, mvgr, mvlag){
  # Get the market value as shown on the assessment roll (which may be lagged)
  # Because of lags, to get assessment-roll market values in the early years, we may need to compute
  # market values for years before year 1, using the growth rate and lages
  # compute pre-year-1 values of mv, based on growth rate and # of lags
  
  # vectors were passed in but we only need the year 1 values for some of them
  mv0 <- mv[1]
  mvgr0 <- mvgr[1]
  mvlag0 <- mvlag[1]
  
  mvlagged <- lag(mv, mvlag0)
  
  # how should we fill in past values -- based on growth rates, or static?
  #mvprior <-  mv0 / (1 + mvgr0) ^ (mvlag0 - 1:length(mv) + 1) # growth rates
  mvprior <-  rep(mv0, length(mv))  # static
  
  mv.ar <- c(mvprior[1:mvlag0], mvlagged[(mvlag0 + 1):length(mvlagged)])
  return(mv.ar)
}



get_tav <- function(av, tav.grcap){
  grcap <- tav.grcap[1]
  tav <- vector(mode="numeric", length=length(av))
  tav[1] <- av[1]
  for(y in 2:length(tav)){
    tav[y] <- pmin(av[y], av[y - 1] * (1 + grcap))
  }
  return(tav)
}

aroll <- aroll.base2 %>%
  group_by(ptype, unit) %>%
  arrange(year) %>%
  mutate(mv.ar=get_mv.ar(mv, mvgr, mvlag),
         av=mv.ar * avratio,
         tav=get_tav(av, tav.grcap)) %>%
  select(year, ptype, unit, mv, mv.ar, av, tav, everything()) %>%
  ungroup %>%
  arrange(ptype, unit, year)
aroll



#****************************************************************************************************
#                results ####
#****************************************************************************************************
aroll %>%
  select(year, ptype, unit, mv, mv.ar, av, tav) %>%
  gather(variable, value, -year, -ptype, -unit) %>%
  mutate(year=as.integer(year),
         variable=factor(variable, levels=c("mv", "mv.ar", "av", "tav"))) %>%
  group_by(ptype, unit, variable) %>%
  mutate(ivalue=value / value[1] * 100) %>%
  ggplot(aes(year, ivalue, colour=variable)) +
    geom_point() + 
    geom_line() + 
    geom_hline(yintercept = 100) +
    scale_x_continuous(breaks=0:20) +
    scale_y_continuous(breaks=seq(100, 200, 10)) +
    facet_wrap(~unit, ncol=2)

         