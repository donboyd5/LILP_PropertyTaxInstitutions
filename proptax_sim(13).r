
# TODO: fix mvar_cycle so that it picks up sales more quickly! (I think)
# - cycle
# - lag
# - portable
# - 


#****************************************************************************************************
#                setup ####
#****************************************************************************************************
source(file.path(PROJHOME, "subprograms", "globals.r"))
source(file.path(PROJHOME, "subprograms", "libraries.r"))
source(file.path(PROJHOME, "subprograms", "functions.r"))


#****************************************************************************************************
#                exploratory functions ####
#****************************************************************************************************
# microbenchmark(
#   get_mv_acquisition(mv, sale, av_grcap),
#   getmva(mv, sale, av_grcap),
#   times=200
# )
build_assessment_roll <- function(assume, globals, nprop){
  # return data frame with data for a single scenario
  
  # unpack the needed globals
  endyear <- globals$endyear # starting from year 1
  burnyears <- globals$burnyears
  totyears <- globals$totyears
  years <- globals$years
  iyears <- globals$iyears
  
  reassess <- create_cycle(assume$avcycle, assume$avcycle_startyear) # reassessment cycle -- 1 or 0
  
  mv_gr <- insert(growth_scenarios[[assume$mv_growth]] / 100, make_vec(.06))
  
  prop_base <- tibble(runname=assume$runname,
                      year=years,
                      index=iyears,
                      reassess=reassess,
                      gr_mv=mv_gr, # growth that occurs IN the current year, to the next
                      cumgr_mv=cumprod(1 + c(0, head(gr_mv, -1))), # cumulative growth to the NEXT year 
                      icumgr_mv=cumgr_mv / cumgr_mv[year==1]) # indexed to year 1
  
  # now create a database of properties -- work with matrices - rows = nprops, cols = years
  
  # matrices of market values (true) - later may update with stochastic growth
  mmv_res_true <- matrix(rep(100 * prop_base$icumgr_mv, nprop),
                         byrow=TRUE, nrow=nprop, ncol=totyears, 
                         dimnames=list(1:nprop, years))
  
  #.. NON-RESIDENTIAL property -- VERY SIMPLE ----
  nonres_mult <- (1 - assume$acqvalue_share) / assume$acqvalue_share
  mmv_nonres <- mmv_res_true * nonres_mult
  # mmv_total <- mmv_res_true + mmv_nonres
  
  #.. RESIDENTIAL property assessment ----
  # matrix of sale indicators
  set.seed(1234)
  msale <- matrix(rbinom(nprop * totyears, 1, assume$sale_fraction), 
                  nrow=nprop, ncol=totyears, 
                  dimnames=list(1:nprop, years))
  zero_years <- NULL
  if(assume$salecycle_zbefore >= min(years)) zero_years <- min(years):(assume$salecycle_zbefore -1)
  if(!is.null(zero_years)) msale[, as.character(zero_years)] <- 0
  
  # colMeans()
  # [1:10, 1:5]
  # matrix of mv on the assessment roll, based on cycle
  mmvar_cycle <- matrix(NA, nrow=nprop, ncol=totyears, dimnames=list(1:nprop, years))
  mmvar_cycle[, 1:assume$armv_lag] <- mmv_res_true[, rep(1, assume$armv_lag)] # fill the initial lags with firstyear mv
  
  # matrix of aquisition market values - if there is a sale use market values, if not use prior plus growth
  # note that this is an "on the assessment roll" concept meaning it can be lagged relative to true market values
  # use lesser of grown prior amv or the mv
  mmvar_acquisition <- matrix(NA, nrow=nprop, ncol=totyears, dimnames=list(1:nprop, years))
  mmvar_acquisition[, 1:assume$armv_lag] <- mmv_res_true[, rep(1, assume$armv_lag)] # fill the initial lags with first of mv
  # mmvar_acquisition[, 1:assume$armv_lag] <- mmv_res_true[, 1:assume$armv_lag] # fill the initial lags with initial years of mv
  
  # fill the residential cycle and acquisition values
  for(j in (assume$armv_lag + 1):totyears){
    mmvar_cycle[, j] <- ifelse(reassess[j]==1, 
                               mmv_res_true[, j - assume$armv_lag], 
                               mmvar_cycle[, j - 1])
    
    mmvar_acquisition[, j] <- ifelse(msale[, j]==1,
                                     mmv_res_true[, j - assume$armv_lag], 
                                     pmin(mmv_res_true[, j - assume$armv_lag], mmvar_acquisition[, j - 1] * (1 +  assume$av_grcap)))
    if(assume$fastdown==1) mmvar_acquisition[, j] <- pmin(mmvar_acquisition[, j], mmv_res_true[, j - 1]) # forces fast downward reassessment
  }
  # mmv_res_true[1:8, 1:10]
  # msale[1:8, 1:10]
  # mmvar_acquisition[1:8, 1:10]
  
  # CAUTION: mvar is market value on the assessment roll -- it can be greater than true market value 
  # because there can be lags
  mdf <- bind_rows(as_tibble(mmv_res_true) %>% mutate(vname="mv_res_true", propid=row_number()),
                   as_tibble(mmv_nonres) %>% mutate(vname="mv_nonres", propid=row_number()),
                   as_tibble(mmvar_cycle) %>% mutate(vname="mvar_cycle", propid=row_number()),
                   as_tibble(msale) %>% mutate(vname="sale", propid=row_number()),
                   as_tibble(mmvar_acquisition) %>% mutate(vname="mvar_acquisition", propid=row_number())) %>%
    gather(year, value, -propid, -vname) %>%
    mutate(year=as.integer(year)) %>%
    spread(vname, value)
  
  prop_all <- prop_base  %>%
    right_join(mdf, by="year") %>%
    select(runname, propid, year, index, sale, reassess, gr_mv, icumgr_mv, mv_nonres, mv_res_true, mvar_cycle, mvar_acquisition)
  
  return(prop_all)
}




#****************************************************************************************************
#                construct mv growth scenarios ####
#****************************************************************************************************
globals
# gr_shock_us <- c(.06, .1, .1, .03, -.02, -.06, -.07, -.03, -.01, .02, .05, .05)

greatrec <- read_csv("./data/greatrec_states.csv")
greatrec %>%
  gather(stabbr, pch, -year, -date) %>%
  filter(stabbr %in% c(globals$css, "US")) %>%
  ggplot(aes(year, pch, colour=stabbr)) +
  geom_line() +
  geom_point()
# looks like years 2003:2017 (15 years) is about right

greatrec_scenarios <- greatrec %>%
  select(-date) %>%
  rename(cyear=year) %>%
  filter(cyear %in% 2002:2017)

growth_scenarios <- greatrec_scenarios
ns(growth_scenarios)


#****************************************************************************************************
#                get run control ####
#****************************************************************************************************
rcdir <- "D:/Dropbox/Open Projects/LILP/PropertyTaxAndInstitutions/ProjectReport/"
rcfn <- "Boyd LILP PropertyTaxInstitutions(6).xlsx"
rcpath <- paste0(rcdir, rcfn)


(rc <- read_excel(rcpath, sheet="run_control", skip = 3))

# for test purposes, pick a row
assume <- as.list(rc[1, ])
assume


#****************************************************************************************************
#                build assessment roll data ####
#****************************************************************************************************

a <- proc.time()
avroll <- rc %>%
  rowwise() %>% 
  do(build_assessment_roll(as.list(.), globals, 1000)) %>%
  ungroup
b <- proc.time()
b - a


#.. create summary of assessment roll ----
avroll_sum <- avroll %>% 
  group_by(runname, year) %>%
  summarise_at(vars(sale, starts_with("mv")), ~sum(.)) %>%
  ungroup %>%
  # CAUTION: For now, use acquisition value for mvar
  # Also, commercial property always assessed timely and accurately
  mutate(mv_total=mv_nonres + mv_res_true,
         mvar_cycle_total=mv_nonres + mvar_cycle,
         mvar_acquisition_total=mv_nonres + mvar_acquisition,
         mvar_total=mvar_acquisition_total) # TEMPORARY
ht(avroll_sum)


#****************************************************************************************************
#                determine tax levies ####
#****************************************************************************************************
# mvar is market value on the assessment roll - for now create it here
avroll_sum_pch <- avroll_sum %>%
  group_by(runname) %>%
  arrange(year) %>%
  mutate_at(vars(starts_with("mv")), list(pch= ~. / lag(.) * 100 - 100))

count(avroll_sum_pch, runname)
avroll_sum_pch  %>%
  select(runname, year, sale, contains("pch")) %>%
  filter(year <= 15, runname=="CA_rules_USGR_20psale")

# calculate cagr in mmv_res_true, to use as desired tax growth rate
mv_cagr <- avroll_sum_pch %>%
  group_by(runname) %>%
  arrange(year) %>%
  summarise(mvfirst=first(mv_total), mvlast=last(mv_total), nyears=max(year) - min(year)) %>%
  mutate(mvcagr=(mvlast / mvfirst)^(1 / nyears) - 1)
mv_cagr

taxlevy <- avroll_sum_pch %>%
  filter(year >= 1) %>%
  left_join(rc %>% select(runname, mvetr_init, mvetr_cap), by="runname") %>%
  group_by(runname) %>%
  mutate(tax_desired=mv_total[year==1] * 
           mvetr_init[year==1] * 
           (1 + mv_cagr$mvcagr[mv_cagr$runname==runname[1]])^(year - 1),
         tax_cap=ifelse(mvetr_cap==1, tax_desired, mvar_total * mvetr_cap),
    tax_levy=pmin(tax_desired, tax_cap))


# do a nice graph, with values indexed either to year=6 (mv_true, tax_desired) or to their counterparts in the given year
# (a) year 6 market value for assessment roll, and (b) year 6 levy
# iyear <- 6
# taxlevy %>%
#   filter(year <= 15, runname=="CA_rules_USGR_5psale") %>%
#   select(year, runname, mv_total, mvar_total, tax_desired, tax_cap, tax_levy) %>%
#   mutate(mvtemp=mv_total, taxtemp=tax_levy) %>%
#   mutate_at(vars(mv_total, mvar_total), ~. / mvtemp[year==iyear] * 100) %>%
#   mutate_at(vars(tax_desired, tax_cap, tax_levy), ~ . / taxtemp[year==iyear] * 100) %>%
#   select(-contains("temp")) %>%
#   gather(vname, value, -runname, -year) %>%
#   ggplot(aes(year, value, colour=vname, linetype=vname)) + # , size=vname
#   geom_line(size=1.5) +
#   geom_hline(yintercept = 100, linetype="dashed") +
#   geom_vline(xintercept = iyear, linetype="dashed") +
#   scale_y_continuous(name="Values indexed", breaks=seq(0, 1000, 10)) +
#   theme_bw()


#.. mv comparisons ----
# do a nice graph, with values indexed either to year=6 (mv_true, tax_desired) or to their counterparts in the given year
# (a) year 6 market value for assessment roll, and (b) year 6 levy
iyear <- 6
ilevy <- taxlevy %>%
  select(year, runname, mv_total, mvar_total, tax_desired, tax_cap, tax_levy) %>%
  group_by(runname) %>%
  mutate(mvtemp=mv_total, taxtemp=tax_levy) %>%
  mutate_at(vars(mv_total, mvar_total), ~. / mvtemp[year==iyear] * 100) %>%
  mutate_at(vars(tax_desired, tax_cap, tax_levy), ~ . / taxtemp[year==iyear] * 100) %>%
  select(-contains("temp")) %>%
  gather(vname, value, -runname, -year) %>%
  ungroup
glimpse(ilevy)
count(ilevy, runname)
count(ilevy, vname)

# compare tax levy and tax cap to desired tax for 3 different runs
#.. Acquisition-value assessing and the residential share ----
count(ilevy, runname)

#.... Market value and market-value on the assessment roll ----
maxyear <- 15

# make a graph with California rules two different turnover rates
xa <- expression(vname=="mvar_total" & str_detect(runname, "CA_rules_"))
xb <- expression(vname=="mv_total" & runname=="CA_rules_USGR_5psale")
levs <- c("mv_total", "CA_rules_USGR_5psale", "CA_rules_USGR_20psale")
labs <- c("Market value", "Assessed value\n5% annual turnover", "Assessed value\n20% annual turnover")
pdata <- ilevy %>%
  filter(eval(xa) | eval(xb)) %>%
  rename(linelabel=runname) %>%
  mutate(linelabel=ifelse(vname=="mv_total", vname, linelabel)) %>%
  select(-vname) %>%
  mutate(linelabel=factor(linelabel, levels=levs, labels=labs)) %>%
  arrange(linelabel)

pca <- pdata %>%
  filter(year <= maxyear) %>%
  ggplot(aes(year, value, colour=linelabel)) +
  geom_line(size=1) +
  geom_hline(yintercept = 100, linetype="dashed") +
  geom_vline(xintercept = iyear, linetype="dashed") +
  scale_x_continuous(breaks=seq(0, 50, 1)) +
  scale_y_continuous(name="Values indexed (True market value=100 in year 6)", breaks=seq(0, 1000, 10)) +
  ggtitle("True market value and assessed market value at 5% and 20% turnover rates",
          subtitle = "Scenario: Market shock, California acquisition-value assessing") +
  theme_bw() +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(.8,.1))
pca 


# now, exclude nonresidential from acquisition-value assessing, use 5% turnover
xa <- expression(vname=="mvar_total" & str_detect(runname, "CA_") & str_detect(runname, "5psale"))
xb <- expression(vname=="mv_total" & runname=="CA_rules_USGR_5psale")
levs <- c("mv_total", "CA_rules_USGR_5psale", "CA_rulesxnonres_USGR_5psale_res30",
          "CA_rulesxnonres_USGR_5psale_res70")
labs <- c("Market value", "Assessed value\nNonresidential participating", 
          "Assessed value\nNonresidential disallowed, 30% nonresidential",
          "Assessed value\nNonresidential disallowed, 70% nonresidential")
pdata <- ilevy %>%
  filter(eval(xa) | eval(xb)) %>%
  rename(linelabel=runname) %>%
  mutate(linelabel=ifelse(vname=="mv_total", vname, linelabel)) %>%
  select(-vname) %>%
  mutate(linelabel=factor(linelabel, levels=levs, labels=labs)) %>%
  arrange(linelabel)

pcaxnonres <- pdata %>%
  filter(year <= maxyear) %>%
  ggplot(aes(year, value, colour=linelabel)) +
  geom_line(size=1) +
  geom_hline(yintercept = 100, linetype="dashed") +
  geom_vline(xintercept = iyear, linetype="dashed") +
  scale_x_continuous(breaks=seq(0, 50, 1)) +
  scale_y_continuous(name="Values indexed (True market value=100 in year 6)", breaks=seq(0, 1000, 10)) +
  ggtitle("True market value and assessed market value with and without nonresidential acquisition value",
          subtitle = "Scenario: Market shock,  acquisition-value assessing") +
  theme_bw() +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(.8,.15))
pcaxnonres

ml <- marrangeGrob(list(pca, pcaxnonres), nrow=2, ncol=1, top=NULL)
ml
ggsave(paste0("./results/", "CArules_mvar_v_mvtrue.png"), ml, scale=1.6, width=6, height=8, units="in")



#.... Tax levy, tax cap, and desired tax levy ----
pdata <- ilevy %>%
  filter(vname %in% c("tax_cap", "tax_levy")) %>%
  filter(runname %in% c("CA5etrcap", "CA5etrcap_lowres", "CA5etrcap_hires")) %>%
  mutate(runname=f(runname))

linedata <- ilevy %>%
  filter(vname=="tax_desired", runname=="CA5etrcap") %>%
  select(year, tax_desired=value)

maxyear <- 15
p <- pdata %>%
  filter(year <= maxyear, vname=="tax_cap") %>%
  ggplot(aes(year, value)) +
  geom_line(aes(colour=runname), size=1) +
  geom_line(aes(year, tax_desired), 
            data=linedata %>% filter(year <= maxyear),
            size=1) +
  geom_hline(yintercept = 100, linetype="dashed") +
  geom_vline(xintercept = iyear, linetype="dashed") +
  scale_x_continuous(breaks=seq(0, 50, 1)) +
  scale_y_continuous(name="Values indexed (Actual levy=100 in year 6)", breaks=seq(0, 1000, 10)) +
  ggtitle("Tax cap and desired tax levy at different acquisition-value shares of tax base",
          subtitle = "Scenario: Market shock, acquisition-value assessing") +
  annotate("text", x = 12, y = 145, label = "Desired\nlevy", size=4) +
  theme_bw() +
  theme(legend.title=element_blank())
p  
ggsave(paste0("./results/", "taxcap_v_desired.png"), p, scale=1.3, width=7, height=6, units="in")

maxyear <- 15
p <- pdata %>%
  filter(year <= maxyear, vname=="tax_levy") %>%
  ggplot(aes(year, value)) +
  geom_line(aes(colour=runname), size=1) +
  geom_line(aes(year, tax_desired), 
            data=linedata %>% filter(year <= maxyear),
            size=1) +
  geom_hline(yintercept = 100, linetype="dashed") +
  geom_vline(xintercept = iyear, linetype="dashed") +
  scale_x_continuous(breaks=seq(0, 50, 1)) +
  scale_y_continuous(name="Values indexed (Actual levy=100 in year 6)", breaks=seq(0, 1000, 10)) +
  ggtitle("Actual tax levy and desired tax levy at different residential shares of tax base",
          subtitle = "Scenario: Market shock, acquisition-value assessing") +
  annotate("text", x = 12, y = 145, label = "Desired\nlevy", size=4) +
  theme_bw() +
  theme(legend.title=element_blank())
p  
ggsave(paste0("./results/", "taxlevy_v_desired.png"), p, scale=1.3, width=7, height=6, units="in")




maxyear <- 15
ilevy %>%
  # filter(runname %in% c("CA20", "CA20etrcap")) %>%
  filter(year <= maxyear) %>%
  filter(str_detect(runname, "etrcap")) %>%
  filter(vname=="tax_levy") %>%
  ggplot(aes(year, value, colour=runname)) +
  geom_line(size=1) +
  geom_line(aes(year, value, colour=vname), 
            data=ilevy %>% filter(year <= maxyear, runname=="CA20", vname=="mv_total"),
            size=1) +
  geom_hline(yintercept = 100, linetype="dashed") +
  geom_vline(xintercept = iyear, linetype="dashed") +
  scale_x_continuous(breaks=seq(0, 50, 2)) +
  scale_y_continuous(name="Values indexed to levy in year 6", breaks=seq(0, 1000, 10)) +
  theme_bw()







taxlevy %>%
  filter(year <= 25) %>%
  select(year, runname, tax_desired, tax_levy) %>%
  gather(vname, value, -runname, -year) %>%
  ggplot(aes(year, value, colour=vname)) + 
  geom_line() +
  facet_wrap(~runname)
  



#****************************************************************************************************
#                analyze results ####
#****************************************************************************************************


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
  filter(str_detect(runname, "30")) %>%
  group_by(runname) %>%
  mutate(mv_true=mv_true / mv_true[year==1] * 100) %>%
  filter(year %in% 1:25) %>%
  ggplot(aes(year, mv_true, colour=runname)) + 
  geom_line() + 
  geom_point() +
  scale_y_continuous(breaks=seq(0, 500, 10)) +
  theme_bw() +
  ggtitle("True market values")

prop_sum %>% 
  filter(year %in% 1:25) %>%
  mutate_at(vars(mvar_acquisition), ~ . / mv_true * 100) %>% # for indexed to mv graph
  ggplot(aes(year, mvar_acquisition, colour=runname)) + 
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = 100) +
  scale_y_continuous(breaks=seq(0, 500, 10)) +
  theme_bw() +
  ggtitle("Acquisition value on assessment roll as % of true market value")

prop_sum %>% 
  filter(year %in% 1:25) %>%
  mutate_at(vars(mvar_cycle), ~ . / mv_true * 100) %>% # for indexed to mv graph
  ggplot(aes(year, mvar_cycle, colour=runname)) + 
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = 100) +
  scale_y_continuous(breaks=seq(0, 500, 10)) +
  theme_bw() +
  ggtitle("Cycle-reassessment market value on assessment roll as % of true market value")

