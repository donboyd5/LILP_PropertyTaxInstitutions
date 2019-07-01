
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
  
  # if(assume$mv_growth=="gr_shock_us") mv_gr <- insert(gr_shock_us, make_vec(.06))
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

avroll_sum_pch  %>%
  select(runname, year, sale, contains("pch")) %>%
  filter(year <= 25, runname=="CA5cap")

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
# f <- function(run){
#   # create factor
#   levs <- c("CA5etrcap_lowres", "CA5etrcap", "CA5etrcap_hires")
#   labs <- c("10% residential", "25% residential", "40% residential")
#   factor(run, levels=levs, labels=labs)
# }

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
