
# TODO: fix mvar_cycle so that it picks up sales more quickly! (I think)


#****************************************************************************************************
#                setup ####
#****************************************************************************************************
source(file.path(PROJHOME, "subprograms", "globals.r"))
source(file.path(PROJHOME, "subprograms", "libraries.r"))
library("multidplyr")

source(file.path(PROJHOME, "subprograms", "functions.r"))


#****************************************************************************************************
#                exploratory functions ####
#****************************************************************************************************
name_vec <- function(vec, vnames=.GlobalEnv$years) {
  names(vec) <- vnames
  return(vec)
}

insert <- function(subvec, vec, start=(.GlobalEnv$burnyears + 1)){
  indexes <- start:(start + length(subvec) - 1)
  vec[indexes] <- subvec
  return(vec)
}


make_vec <- function(value) {
  vec <- rep(value, .GlobalEnv$totyears)
  vec <- name_vec(vec)
  return(vec)
}
# make_vec(.06)

create_cycle <- function(clength, cstart, zbefore=NULL){
  # returns a named vector with 1 in the years in which the cycle event is occurring
  # clength is the length of the cycle -- e.g., 3 means reassess every 3 years
  # cstart is the first year > 0 we'd like to see the cycle in
  # if burn==FALSE, zero-out the burn years
  cycle <- make_vec(0)
  icycle <- which((.GlobalEnv$years - cstart) %% clength == 0) # starts at 0 when cstart=0
  cycle[icycle] <- 1
  if(hasArg(zbefore)) cycle[which(.GlobalEnv$years < zbefore)] <- 0
  return(cycle)
}
# create_cycle(7, 40)
# create_cycle(7, 5, zbefore=5)

# microbenchmark(
#   get_mv_acquisition(mv, sale, av_grcap),
#   getmva(mv, sale, av_grcap),
#   times=200
# )


get_mv_cycle <- function(mv, reassess, armv_lag){
  mv_cycle <- make_vec(0)[1:length(mv)]
  mv_cycle[1:armv_lag] <- mv[1]
  for(i in (armv_lag + 1):length(mv)) {
    # i <- armv_lag + 1
    # i <- i + 1
    mv_cycle[i] <- ifelse(reassess[i]==1, 
                          mv[i - armv_lag], 
                          mv_cycle[i - 1])
    # cbind(reassess, mv, mv_cycle)
  }
  return(mv_cycle)
}


#****************************************************************************************************
#                additional globals ####
#****************************************************************************************************
gr_shock_us <- c(.06, .1, .1, .03, -.02, -.06, -.07, -.03, -.01, .02, .05, .05)


#****************************************************************************************************
#                get run control ####
#****************************************************************************************************
rcdir <- "D:/Dropbox/Open Projects/LILP/PropertyTaxAndInstitutions/ProjectReport/"
rcfn <- "Boyd LILP PropertyTaxInstitutions(4).xlsx"
# Boyd LILP PropertyTaxInstitutions(4).xlsx
rcpath <- paste0(rcdir, rcfn)

(rc <- read_excel(rcpath, sheet="run_control"))

assume <- as.list(rc[3, ])
assume

assume <- as.list(rc[3, ])
assume

build_data <- function(assume, nprop){
  # return data frame with data for a single scenario
  endyear <- 50 # starting from year 1
  burnyears <- 11
  totyears <- endyear + burnyears
  years <- (-burnyears+1):endyear
  iyears <- name_vec(1:totyears)
  
  reassess <- create_cycle(assume$avcycle, assume$avcycle_startyear) # reassessment cycle -- 1 or 0
  
  if(assume$mv_growth=="gr_shock_us") mv_gr <- insert(gr_shock_us, make_vec(.06))
  
  prop_base <- tibble(runname=assume$runname,
                      year=years,
                      index=iyears,
                      reassess=reassess,
                      gr_mv=mv_gr, # growth that occurs IN the current year, to the next
                      cumgr_mv=cumprod(1 + c(0, head(gr_mv, -1))), # cumulative growth to the NEXT year 
                      icumgr_mv=cumgr_mv / cumgr_mv[year==1]) # indexed to year 1

  # now create a database of properties
  
  # work with matrices - rows = nprops, cols = years
  
  # matrix of sale indicators
  set.seed(1234)
  msale <- matrix(rbinom(nprop * totyears, 1, assume$sale_fraction), nrow=nprop, ncol=totyears, dimnames=list(1:nprop, years))
  zero_years <- NULL
  if(assume$salecycle_zbefore >= min(years)) zero_years <- min(years):(assume$salecycle_zbefore -1)
  if(!is.null(zero_years)) msale[, as.character(zero_years)] <- 0
  
  # matrix of market values (true) - later may update with stochastic growth
  mmv_true <- matrix(rep(100 * prop_base$icumgr_mv, nprop), byrow=TRUE, nrow=nprop, ncol=totyears, dimnames=list(1:nprop, years))
  # colMeans()
  # [1:10, 1:5]
  
  # matrix of mv on the assessment roll, based on cycle
  mmvar_cycle <- matrix(NA, nrow=nprop, ncol=totyears, dimnames=list(1:nprop, years))
  mmvar_cycle[, 1:assume$armv_lag] <- mmv_true[, rep(1, assume$armv_lag)] # fill the initial lags with firstyear mv
  
  # matrix of aquisition market values - if there is a sale use market values, if not use prior plus growth
  # note that this is an "on the assessment roll" concept meaning it can be lagged relative to true market values
  # use lesser of grown prior amv or the mv
  mamv <- matrix(NA, nrow=nprop, ncol=totyears, dimnames=list(1:nprop, years))
  mamv[, 1:assume$armv_lag] <- mmv_true[, rep(1, assume$armv_lag)] # fill the initial lags with firstyear mv
  
  # fill the cycle and acquisition values
  for(j in (assume$armv_lag + 1):totyears){
    mmvar_cycle[, j] <- ifelse(reassess[j]==1, 
                               mmv_true[, j - assume$armv_lag], 
                               mmvar_cycle[, j - 1])
    
    mamv[, j] <- ifelse(msale[, j]==1,
                        mmv_true[, j - assume$armv_lag], 
                        pmin(mmv_true[, j - assume$armv_lag], mamv[, j - 1] * (1 +  assume$av_grcap)))
  }
  # mmv_true[1:8, 1:10]
  # msale[1:8, 1:10]
  # mamv[1:8, 1:10]
  
  # CAUTION: mvar is market value on the assessment roll -- it can be greater than true market value 
  # because there can be lags
  mdf <- bind_rows(as_tibble(mmv_true) %>% mutate(vname="mv_true", propid=row_number()),
                   as_tibble(mmvar_cycle) %>% mutate(vname="mvar_cycle", propid=row_number()),
                   as_tibble(msale) %>% mutate(vname="sale", propid=row_number()),
                   as_tibble(mamv) %>% mutate(vname="mvar_acquisition", propid=row_number())) %>%
    gather(year, value, -propid, -vname) %>%
    mutate(year=as.integer(year)) %>%
    spread(vname, value)
  
  prop_all <- prop_base  %>%
    right_join(mdf, by="year") %>%
    select(runname, propid, year, index, sale, reassess, gr_mv, icumgr_mv, mv_true, mvar_cycle, mvar_acquisition)
  
  return(prop_all)
}

a <- proc.time()
simrun <- rc %>%
  rowwise() %>% 
  do(build_data(as.list(.), 1000)) %>%
  ungroup
b <- proc.time()
b - a

# check <- simrun %>%
#   filter(runname=="shock_30pct", propid==3)

simrun %>% ht
simrun %>%
  filter(propid==1) %>%
  select(runname, year, propid, value=sale) %>%
  spread(runname, value) %>%
  head

prop_sum <- simrun %>% 
  group_by(runname, year) %>%
  summarise_at(vars(sale, starts_with("mv")), ~sum(.)) %>%
  ungroup
ht(prop_sum)

prop_sum %>% 
  filter(year %in% 1:25) %>%
  mutate_at(vars(mvar_acquisition), ~ . / mv_true * 100) %>% # for indexed to mv graph
  ggplot(aes(year, mvar_acquisition, colour=runname)) + 
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = 100) +
  scale_y_continuous(breaks=seq(0, 500, 10)) +
  theme_bw()

prop_sum %>% 
  filter(year %in% 1:25) %>%
  mutate_at(vars(mvar_cycle), ~ . / mv_true * 100) %>% # for indexed to mv graph
  ggplot(aes(year, mvar_cycle, colour=runname)) + 
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = 100) +
  scale_y_continuous(breaks=seq(0, 500, 10)) +
  theme_bw()

# prop_sum %>% 
#   filter(year %in% 1:25) %>% 
#   select(year, starts_with("mv_")) %>%
#   mutate_at(vars(-year, -mv_boy), ~ . / mv_boy * 100) %>% # for indexed to mv graph
#   pivot_longer(-year) %>%
#   mutate(name=factor(name, levels=c("mv_true", "mvar_acquisition", "mv_avcycle"))) %>%
#   filter(name!="mv_boy") %>%
#   ggplot(aes(year, value, colour=name)) + 
#   geom_line() + 
#   geom_point() +
#   geom_hline(yintercept = 100) +
#   scale_color_manual(values=c("blue", "#fc8d59", "#fee08b")) +
#   scale_y_continuous(breaks=seq(0, 500, 20), limits=c(50, 150)) +
#   theme_bw()


#****************************************************************************************************
#                OLD: run the model ####
#****************************************************************************************************
source(file.path(PROJHOME, "subprograms", "get_assumptions.r"))

glimpse(runset)
count(runset, fullname)
count(runset, fullname, ptype, unit)

# first, calculate the assessment roll
results.assessroll <- runset %>%
  # first do calcs that can be done on ungrouped data
  mutate(mv.true=mv0 * mvg.factor) %>%  
  # now do within-group calcs
  group_by(fullname, growthratesname, rulesname, taxbasename, ptype, unit) %>%
  arrange(year) %>%
  mutate(reassess.year = get_reassess.year(year, initial.reassess.year, assess.cycle),
         mv.known=get_mv.known(mv.true, reassess.year),
         mv.on.roll=get_mv.on.roll(mv.known, adjust.period, reassess.year),
         av=avratio * mv.on.roll,
         av.taxable=get_av.taxable(av, tav.grcap)) %>%
  ungroup
glimpse(results.assessroll)


# now calculate tax rate, which we assume must be done for the entire tax base rather than for individual properties --
# that is, one rate for all properties

get_levy <- function(df){
  # do this within a scenario
  
  df <- df %>%
    mutate(av.taxable.growth=av.taxable / av.taxable[match(year - 1, year)] - 1,
           chosen.levy.growth=av.taxable.growth + (rev.goal - av.taxable.growth) * taxrate.response)
  
  df$levy.total <- rep(NA_real_, nrow(df))
  df$levy.total[1] <- .02 * df$av.taxable[1] # initial av tax rate is 2% -- for now, fine to keep that for everything
  for(i in 2:length(df$levy.total)){
    df$levy.total[i] <- df$levy.total[i - 1] * (1 + df$chosen.levy.growth[i])
  }
  return(df)
}


levy <- results.assessroll %>%
  group_by(fullname, year) %>% # temporary solution - keep rules name
  summarise_at(vars(mv.on.roll, av, av.taxable), ~sum(. * weight)) %>%
  left_join(results.assessroll %>% select(fullname, rev.goal, taxrate.response) %>% unique) %>%
  do(get_levy(.)) %>%
  mutate(taxrate.av= levy.total / av.taxable) %>%
  mutate(check=levy.total / levy.total[match(year - 1, year)] - 1) %>%
  ungroup
glimpse(levy)

# now we can compute the tax roll, including tax levy and tax rate

results.taxroll <- results.assessroll %>%
  left_join(levy %>% select(fullname, year, chosen.levy.growth, levy.total, taxrate.av)) %>%
  mutate(levy=av.taxable * taxrate.av) # for this specific property

glimpse(results.taxroll)
count(results.taxroll, fullname)

tmp <- results.assessroll %>%
  filter(runname %in% c("medium.midres", "fast.midres"),
         ptype=="res",
         unit==1,
         year<=6) %>%
  select(fullname, year, mvgr, assess.cycle, initial.reassess.year, reassess.year, adjust.period,
         mv.true, mv.known, mv.on.roll, av, av.taxable) %>%
  arrange(fullname, year)


#****************************************************************************************************
#                examine results ####
#****************************************************************************************************
# put scenarios in desired output order for graphing, etc
results.taxroll %>% select(contains("name")) %>% unique
count(results.taxroll, fullname)

growth.sort <- c("shock")
rulesav.sort <- c("adjust.slow", "adjust.medium", "adjust.fast") %>% rev
ruleslevy.sort <- c("respond.strong.80pctvs4", "respond.medium.40pctvs4", "respond.none")
base.sort <- c("20pct.res", "50pct.res", "80pct.res")

sortnames <- runs %>%
  mutate(growthratesname=factor(growthratesname, levels=growth.sort),
         rulesname.av=factor(rulesname.av, levels=rulesav.sort),
         rulesname.levy=factor(rulesname.levy, levels=ruleslevy.sort),
         taxbasename=factor(taxbasename, levels=base.sort),
         shortname=str_remove(rulesname.av, "adjust.")) %>%
  arrange(growthratesname, rulesname.av, rulesname.levy, taxbasename)
sortnames

scenarios <- sortnames$fullname

runnames <- c("slow.midres", "medium.midres", "fast.midres")

pdata <- results.taxroll %>%
  filter(year <= 15, runname %in% runnames) %>%
  group_by(fullname, year) %>%
  summarise_at(vars(mv.true, mv.known, mv.on.roll, av, av.taxable, levy), ~sum(. * weight)) %>%
  left_join(levy %>% select(fullname, year, taxrate.av)) %>%
  gather(variable, value, -fullname, -year) %>%
  mutate(variable=factor(variable, levels=c("mv.true", "mv.known", "mv.on.roll", "av", "av.taxable", "taxrate.av", "levy"))) %>%
  group_by(fullname, variable) %>%
  mutate(ivalue=value / value[1] * 100) %>%
  ungroup


mvscenario <- "3 years of declines (7.5%, 10%, 5%), then 3 years of 3% growth, then 5% annual growth"

# graph values
pdata2 <- pdata %>%
  filter(variable %in% c("mv.true", "mv.known", "mv.on.roll", "av", "av.taxable")) %>%
  filter(str_detect(fullname, "50pct"))
top <- pdata2 %>% .[["ivalue"]] %>% max
lab1 <- "1-year AV cycle\n1-year MV phase-in for all\n6% res. growth cap"
lab2 <- "3-year AV cycle\n2-year res. MV phase-in, 1-year comm.\n4% res. growth cap"
lab3 <- "5-year AV cycle\n3-year res. MV phase-in, 1-year comm.\n2% res. growth cap"
labs <- c(lab1, lab2, lab3)

p1 <- pdata2 %>%
  mutate(shortname=factor(fullname, levels=sortnames$fullname, labels=paste0("Assessed value adjustment process: ", sortnames$shortname))) %>%
  ggplot(aes(year, ivalue, colour=variable)) +
  geom_point() + 
  geom_line(size=1.25) + 
  geom_hline(yintercept = 100) +
  scale_x_continuous(breaks=0:20) +
  scale_y_continuous(name="Value as % of year-1 value", breaks=seq(0, 200, 5)) +
  facet_wrap(~shortname, ncol=1) +
  annotate("label", x=1, y=top - 1, label=labs, hjust=0, vjust=1, size=3) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  ggtitle("Property value measures under different adjustment rules\nResidential MV share=50%. AV ratios: Commercial 100%, Residential 40%",
          subtitle=paste0("Market value scenario: ", mvscenario))
p1
ggsave("./results/values.png", plot=p1, width = 9, height = 12, units="in")


# graph taxable value, tax rate, and levy
pdata2 <- pdata %>%
  filter(variable %in% c("mv.true", "av.taxable", "taxrate.av", "levy")) %>%
  filter(str_detect(fullname, "50pct"))
top <- pdata2 %>% .[["ivalue"]] %>% max
lab1 <- "1-year AV cycle\n1-year MV phase-in for all\n6% res. growth cap\n0% rate response"
lab2 <- "3-year AV cycle\n2-year res. MV phase-in, 1-year comm.\n4% res. growth cap\n40% rate response"
lab3 <- "5-year AV cycle\n3-year res. MV phase-in, 1-year comm.\n2% res. growth cap\n80% rate response"
labs <- c(lab1, lab2, lab3)
p2 <- pdata2 %>%
  mutate(shortname=factor(fullname, levels=sortnames$fullname, labels=paste0("Assessed value adjustment process: ", sortnames$shortname))) %>%
  ggplot(aes(year, ivalue, colour=variable)) +
  geom_point() + 
  geom_line(size=1.25) + 
  geom_hline(yintercept = 100) +
  scale_x_continuous(breaks=0:20) +
  scale_y_continuous(name="Value as % of year-1 value", breaks=seq(0, 200, 5)) +
  facet_wrap(~shortname, ncol=1) +
  annotate("label", x=1, y=top - 1, label=labs, hjust=0, vjust=1, size=3) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  ggtitle("Tax rate and levy response under different adjustment rules\nResidential MV share=50%. AV ratios: Commercial 100%, Residential 40%",
          subtitle=paste0("Market value scenario: ", mvscenario))
p2
ggsave("./results/response.png", plot=p2, width = 9, height = 12, units="in")


# pdata %>%
#   filter(str_detect(fullname, "slow")) %>%
#   ggplot(aes(year, ivalue, colour=variable)) +
#   geom_point() + 
#   geom_line() + 
#   geom_hline(yintercept = 100) +
#   scale_x_continuous(breaks=0:20) +
#   scale_y_continuous(name="Value as % of year-1 value", breaks=seq(0, 200, 5)) +
#   facet_wrap(~fullname, ncol=1) +
#   # annotate("text", x=1, y=125, label=labs) +
#   theme_bw() +
#   ggtitle("Property value measures under different tax-base compositions, adjustment rules=slow",
#           subtitle="Residential share increases as we move from top to bottom")


# pdata %>%
#   ggplot(aes(year, ivalue, colour=variable)) +
#   geom_point() + 
#   geom_line() + 
#   geom_hline(yintercept = 100) +
#   scale_x_continuous(breaks=0:20) +
#   scale_y_continuous(name="Value as % of year-1 value", breaks=seq(0, 200, 5)) +
#   facet_wrap(~fullname, ncol=3) +
#   theme_bw() +
#   ggtitle("Property value measures under different rules and tax-base compositions",
#           subtitle="Residential share increases as we move from left panels to right\nInstitutional adjustment process slows as we move from top to bottom")


#****************************************************************************************************
#                diagnostics ####
#****************************************************************************************************

# what's happening to mv.ar with basic-shock-medium??
tmp <- results.taxroll %>% 
  filter(str_detect(fullname, "shock-adjust.slow")) %>%
  filter(ptype=="res", unit==1, year<=15) %>%
  select(taxbasename, ptype, unit, year, av, tav.grcap, av.taxable) %>%
  group_by(taxbasename, ptype, unit) %>%
  arrange(year) %>%
  mutate_at(vars(av, av.taxable), list(pch= ~ . / lag(.) * 100 - 100))
glimpse(tmp)  
  
# spread(fullname, ivalue) %>%
# mutate(diff=.[[4]] - .[[3]]) %>%
# as.data.frame
# select(fullname, year, assess.cycle, initial.reassess.year, reassess.year, adjust.period, mv.true, mv.known, mv.on.roll)

  




write_xlsx(tmp, "./results/tmp.xlsx", col_names = TRUE)
