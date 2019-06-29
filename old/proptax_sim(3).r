


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
rcfn <- "RunControlPropTax(3).xlsx"


#****************************************************************************************************
#                get assumptions ####
#****************************************************************************************************

taxbases <- read_excel(paste0(rcdir, rcfn),
                       sheet = "taxbases",
                       range = cell_rows(c(4, NA)))

taxbases <- taxbases %>%
  filter(!is.na(taxbasename)) %>%
  group_by(taxbasename, ptype) %>%
  mutate(unit=row_number()) %>%
  ungroup
taxbases


rules <- read_excel(paste0(rcdir, rcfn),
                    sheet = "rules",
                    range = cell_rows(c(4, NA)))
rules <- rules %>%
  filter(!is.na(rulesname))
rules


growthrates <- read_excel(paste0(rcdir, rcfn),
                          sheet = "growthrates",
                          range = cell_rows(c(4, NA)))
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
  mutate(mvg.factor=cumprod(1 + c(0, head(mvgr, -1)))) # shift mvgr to the right and insert 0
ht(growthrates_expanded)

growthrates_expanded %>% slice(c(1:3, (n() - 2):n()))

growthrates_expanded %>%
 ggplot(aes(year, mvg.factor, colour=growthratesname)) + geom_line() + geom_point() + geom_hline(yintercept = 1)

runs <- read_excel(paste0(rcdir, rcfn),
                    sheet = "runs",
                    range = cell_rows(c(4, NA))) %>%
  mutate(fullname=paste0(taxbasename, "-", growthratesname, "-", rulesname))
runs

runset <- 
  runs %>%
  left_join(growthrates_expanded) %>%
  left_join(rules) %>%
  left_join(taxbases) %>%
  mutate(mv=mv0 * mvg.factor) %>%
  arrange(runname, ptype, unit)
glimpse(runset)

count(runset, runname, fullname)

count(runset, runname, fullname, ptype, unit)


#****************************************************************************************************
#                run the model ####
#****************************************************************************************************
get_mv.arbase <- function(mv, mvgr, assess.cycle){
  # Get the market value upon which the assessment roll will be based
  # The only difference between this market value and the true market value is the assessment lag
  # In other words, this is what was true market valueshown on the assessment roll before rea
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



#****************************************************************************************************
#                results ####
#****************************************************************************************************

glimpse(runset)
count(runset, fullname)

results <- runset %>%
  group_by(runname, fullname, taxbasename, growthratesname, rulesname, ptype, unit) %>%
  arrange(year) %>%
  mutate(mv.ar=get_mv.ar(mv, mvgr, mvlag), # market value on the assessment roll, after lags
         av=mv.ar * avratio,  # assessed value, reflecting assessment ratio
         tav=get_tav(av, tav.grcap)) %>%  # taxable assessed value, reflecting tax rates
  select(year, ptype, unit, mv, mv.ar, av, tav, everything()) %>%
  ungroup %>%
  arrange(ptype, unit, year)

glimpse(results)


results %>%
  filter(year <= 15) %>%
  group_by(runname, fullname, year) %>%
  summarise_at(vars(mv, mv.ar, av, tav), funs(sum(. * weight))) %>%
  gather(variable, value, -runname, -fullname, -year) %>%
  group_by(runname, fullname, variable) %>%
  mutate(ivalue=value / value[1] * 100) %>%
  ggplot(aes(year, ivalue, colour=variable)) +
    geom_point() + 
    geom_line() + 
    geom_hline(yintercept = 100) +
    scale_x_continuous(breaks=0:20) +
    scale_y_continuous(breaks=seq(0, 200, 5)) +
    facet_wrap(~fullname, ncol=1)

         