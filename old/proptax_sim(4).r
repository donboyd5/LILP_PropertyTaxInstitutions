


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
library("writexl")
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
rcfn <- "RunControlPropTax(4).xlsx"


#****************************************************************************************************
#                get assumptions ####
#****************************************************************************************************

#.. get market-value growth scenario ----
growthrates <- read_excel(paste0(rcdir, rcfn),
                          sheet = "growthrates",
                          range = cell_rows(c(4, NA)))
growthrates <- growthrates %>%
  filter(!is.na(growthratesname))
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
  group_by(growthratesname) %>% # assumes the data will be correct - we only have one "from" group
  do(expand_years(.)) %>%
  mutate(mvg.factor=cumprod(1 + c(0, head(mvgr, -1)))) # shift mvgr to the right and insert 0
ht(growthrates_expanded)

growthrates_expanded %>% slice(c(1:3, (n() - 2):n()))

growthrates_expanded %>%
  ggplot(aes(year, mvg.factor, colour=growthratesname)) + geom_line() + geom_point() + geom_hline(yintercept = 1)


#.. get institutional rules and practices ----

rules <- read_excel(paste0(rcdir, rcfn),
                    sheet = "rules",
                    range = cell_rows(c(4, NA)))
rules <- rules %>%
  filter(!is.na(rulesname))
rules


#.. get tax base scenarios ----

taxbases <- read_excel(paste0(rcdir, rcfn),
                       sheet = "taxbases",
                       range = cell_rows(c(4, NA)))

taxbases <- taxbases %>%
  filter(!is.na(taxbasename)) %>%
  group_by(taxbasename, ptype) %>%
  mutate(unit=row_number()) %>%
  ungroup
taxbases


#.. put run information together and create runset ----
runs <- read_excel(paste0(rcdir, rcfn),
                    sheet = "runs",
                    range = cell_rows(c(4, NA))) %>%
  mutate(fullname=paste0(growthratesname, "-", rulesname, "-", taxbasename))
runs


runset <- 
  runs %>%
  left_join(growthrates_expanded) %>%
  left_join(rules) %>%
  left_join(taxbases) %>%
  arrange(runname, ptype, unit)
glimpse(runset)


count(runset, runname, fullname)

count(runset, runname, fullname, ptype, unit)


#****************************************************************************************************
#                functions needed to run the model ####
#****************************************************************************************************


get_reassess.year <- function(year, initial.reassess.year, assess.cycle){
  # construct a logical vector that is true in reassessment years, false otherwise
  reassess.year <- {(year - initial.reassess.year) %% assess.cycle} == 0 # %% gives the remainder
  return(reassess.year)
}


get_mv.known <- function(mv.true, reassess.year){
  # Get the market value upon which the assessment roll will be based
  # The only difference between this market value and the true market value is the assessment lag
  
  # This base value is NOT necessarily the mv shown on the roll. For example, an assessor might not reflect
  # the full change in mv immediately after a drop. But this is the information gathered upon
  # which the mv on the roll will be based
  
  # The code below assumes that in a reassessment year, the new base mv will be last year's mv, and in
  # other years it will simply be carried forward
  
  # if assess.cycle=1, we reassess every year, so mv.known=mv lagged 1 year
  # if assess.cycle=2, we reassess every other year, so we retain the mv for 2 years, etc.
  
  # we have to establish what mv was in the prior years
  # for now assume it was the same as in year 1
  
  # mv.prior <- rep(mv[1], assess.cycle)
  # mv.all <- c(mv.prior, mv)
  # reassess.year <- get_reassess.year(year, initial.reassess.year, assess.cycle)
  
  mv.known <- rep(NA_real_, length(mv.true))  
  mv.known[reassess.year] <- lag(mv.true)[reassess.year]
  mv.known[1] <- mv.true[1] # IMPROVE ON THIS! -- fill initial year of mv.known with the market value from year 1
  for(y in 2:length(mv.known)) if(is.na(mv.known[y])) mv.known[y] <- mv.known[y-1]
  
  return(mv.known)
}


get_mv.on.ar <- function(mv.known, adjust.period, reassess.year){
  # Get the market value as shown on the assessment roll
  # it is based on the mv.known but may be phased in over several years
  # (think about whether this really is realistic assessor behavior)
  
  # in each reassessment year, the assessor looks a the new mv they have mv.known (which is prior year's mv, lag(mv))
  # compares it to last year's mv on the assessment roll, lag(mv.ar)
  # computes how much it has gone up or down, and divides by the adjustment period
  # then adds that to lag(mv.ar) each year until the next 
  # reassessment year, at which point process repeats
  
  # test data
  # year <- 1:12
  # initial.reassess.year.scalar <- 0
  # assess.cycle.scalar <- 1
  # adjust.period.scalar <- 1
  # 
  # initial.reassess.year <- rep(initial.reassess.year.scalar, length(year))
  # assess.cycle <- rep(assess.cycle.scalar, length(year))
  # adjust.period <- rep(adjust.period.scalar, length(year))
  # 
  # reassess.year <- get_reassess.year(year, initial.reassess.year, assess.cycle)
  # mv <- seq(100, 210, 10)
  # mv.known <- get_mv.known(mv, reassess.year) # c(100, 100, rep(110, 3), rep(140, 3), rep(170, 3), 200)
  # cbind(reassess.year, mv, mv.known)
  
  # mv.ar for 1:12, 0, 3, 4should be:
  # 100.0000 100.0000 102.5000 105.0000 107.5000 115.6250 123.7500 131.8750 141.4062 150.9375 160.4688 170.3516
  
  # Easiest way to do this is with a loop; think about whether it can be vectorized
  # print(cbind(reassess.year, adjust.period))
  
  adjust.period <- adjust.period[1]
  mv.on.ar <- rep(NA_real_, length(mv.known))
  first.reassess.year <- min(which(reassess.year == TRUE))
  
  mv.on.ar[1:(first.reassess.year - 1)] <- mv.known[1:(first.reassess.year - 1)]
  
  if(first.reassess.year==1) start.year <- 2 else start.year <- first.reassess.year
  
  for(y in start.year:length(mv.on.ar)){
    if(reassess.year[y]) {
      mv.total.diff <- mv.known[y] - mv.on.ar[y - 1]
      mv.annual.increment <- mv.total.diff / adjust.period
      periods.adjusted <- 0
    }
    periods.adjusted <- periods.adjusted + 1
    if(periods.adjusted > adjust.period) mv.annual.increment <- 0
    mv.on.ar[y] <- mv.on.ar[y - 1] + mv.annual.increment
  }
  # cbind(year, reassess.year, mv, mv.known, mv.ar)
  
  # may need some error checking for incorrect inputs
  return(mv.on.ar)
}


get_av.taxable <- function(av, tav.grcap){
  # taxable assessed value
  grcap <- tav.grcap[1]
  av.taxable <-  rep(NA_real_, length(av))
  av.taxable[1] <- av[1]
  for(y in 2:length(av.taxable)){
    av.taxable[y] <- pmin(av[y], av.taxable[y - 1] * (1 + grcap))
  }
  return(av.taxable)
}



#****************************************************************************************************
#                run the model ####
#****************************************************************************************************

glimpse(runset)
count(runset, fullname)


results <- runset %>%
  # first do calcs that can be done on ungrouped data
  mutate(mv.true=mv0 * mvg.factor) %>%  
  # now do within-group calcs
  group_by(runname, fullname, taxbasename, growthratesname, rulesname, ptype, unit) %>%
  arrange(year) %>%
  mutate(reassess.year = get_reassess.year(year, initial.reassess.year, assess.cycle),
         mv.known=get_mv.known(mv.true, reassess.year),
         mv.on.ar=get_mv.on.ar(mv.known, adjust.period, reassess.year),
         av=avratio * mv.on.ar,
         av.taxable=get_av.taxable(av, tav.grcap)) %>%
  ungroup
glimpse(results)
count(results, fullname)

# put scenarios in desired output order
scenarios <- c("shock-adjust.slow-60pct.res", "shock-adjust.medium-60pct.res",
               "shock-adjust.fast-60pct.res", "shock-adjust.slow-16pct.res")

pdata <- results %>%
  filter(year <= 15) %>%
  mutate(fullname=factor(fullname, levels=scenarios)) %>%
  group_by(runname, fullname, year) %>%
  summarise_at(vars(mv.true, mv.known, mv.on.ar, av, av.taxable), funs(sum(. * weight))) %>%
  gather(variable, value, -runname, -fullname, -year) %>%
  mutate(variable=factor(variable, levels=c("mv.true", "mv.known", "mv.on.ar", "av", "av.taxable"))) %>%
  group_by(runname, fullname, variable) %>%
  mutate(ivalue=value / value[1] * 100) %>%
  ungroup

pdata %>%
  ggplot(aes(year, ivalue, colour=variable)) +
    geom_point() + 
    geom_line() + 
    geom_hline(yintercept = 100) +
    scale_x_continuous(breaks=0:20) +
    scale_y_continuous(breaks=seq(0, 200, 5)) +
    facet_wrap(~fullname, ncol=2)

# why don't we see a diff between shock-slow-60 and shock-slow-16??
pdata %>% filter(fullname %in% scenarios[c(1, 4)]) %>%
  select(-value, -runname) %>%
  spread(fullname, ivalue) %>%
  mutate(diff=.[[4]] - .[[3]]) %>%
  as.data.frame


#****************************************************************************************************
#                diagnostics ####
#****************************************************************************************************
         
# what's happening to mv.ar with basic-shock-medium??
tmp <- results %>% 
  filter(str_detect(fullname, "shock-adjust.slow")) %>%
  filter(ptype=="res", unit==1, year<=15) %>%
  select(taxbasename, ptype, unit, year, av, tav.grcap, av.taxable) %>%
  group_by(taxbasename, ptype, unit) %>%
  arrange(year) %>%
  mutate_at(vars(av, av.taxable), funs(pch=. / lag(.) * 100 - 100))
glimpse(tmp)  
  
  # spread(fullname, ivalue) %>%
  # mutate(diff=.[[4]] - .[[3]]) %>%
  # as.data.frame
  # select(fullname, year, assess.cycle, initial.reassess.year, reassess.year, adjust.period, mv.true, mv.known, mv.on.ar)

  




write_xlsx(tmp, "./results/tmp.xlsx", col_names = TRUE)
