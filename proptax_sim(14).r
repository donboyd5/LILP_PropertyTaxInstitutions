
# TODO: fix mvar_cycle so that it picks up sales more quickly! (I think)
# - cycle
# - lag
# - portable
# - 


#****************************************************************************************************
#                setup ####
#****************************************************************************************************
source(file.path(PROJHOME, "subprograms", "libraries.r"))

source(file.path(PROJHOME, "subprograms", "globals.r"))
source(file.path(PROJHOME, "subprograms", "functions.r"))
source(file.path(PROJHOME, "subprograms", "functions_assessment_roll.r"))


#****************************************************************************************************
#                exploratory functions ####
#****************************************************************************************************
# microbenchmark(
#   get_mv_acquisition(mv, sale, av_grcap),
#   getmva(mv, sale, av_grcap),
#   times=200
# )


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
  geom_point() +
  geom_hline(yintercept = 0)
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


(rc <- read_excel(rcpath, sheet="run_control", skip = 4))
# for test purposes, pick a row
# (assume <- as.list(rc[1, ]))

#****************************************************************************************************
#                build assessment roll data ####
#****************************************************************************************************
rcuse <- rc %>%
  filter(include==1)
# rcuse
rcuse$runname
rcuse$avcycle_baseyear
(assume <- as.list(rcuse[1, ]))

a <- proc.time()
avroll <- rcuse %>%
  rowwise() %>% # each row is a scenario
  do(build_assessment_roll(as.list(.), globals, growth_scenarios)) %>%
  ungroup
b <- proc.time()
b - a
# ht(avroll)
avroll %>%
  group_by(ptype) %>%
  summarise(n=n() / 61)


#.. create summary of assessment roll ----
glimpse(avroll)

avroll_sum <- avroll %>% 
  group_by(runname, year) %>%
  summarise_at(vars(starts_with("mvtrue"), av_acquisition_sale_year, starts_with("av_acq"), reassess_year, av_cycle),
               ~sum(., na.rm=TRUE)) %>%
  mutate(av_total=av_acquisition + av_cycle) %>%
  ungroup
ht(avroll_sum)


avroll_sum %>%
  filter(year %in% 1:20) %>%
  select(year, runname, mvtrue, av_total) %>%
  gather(vname, value, -year, -runname) %>%
  ggplot(aes(year, value, colour=vname)) +
  geom_line() +
  geom_hline(yintercept = 100) +
  scale_x_continuous(breaks=seq(0, 40, 2)) +
  facet_wrap(~runname, ncol=1)

avroll_sum %>%
  filter(year %in% 1:20) %>%
  filter(str_sub(runname, 3, 3)!="_") %>%
  select(year, runname, av_total) %>%
  ggplot(aes(year, av_total, colour=runname)) +
  geom_line() +
  geom_point()

avroll_sum %>%
  filter(year %in% 1:20) %>%
  select(year, runname, mvtrue, av_total) %>%
  mutate(av_pct=av_total / mvtrue * 100) %>%
  ggplot(aes(year, av_pct, colour=runname)) +
  geom_line() +
  scale_x_continuous(breaks=seq(0, 30, 2)) +
  scale_y_continuous(breaks=seq(0, 200, 5)) +
  geom_hline(yintercept = 100)


#.. construct measures of volatility and rise and fall ----
avroll_sum %>%
  filter(year %in% 1:20) %>%
  select(runname, year, mvtrue, av_total) %>%
  gather(vname, value, -runname, -year) %>%
  arrange(runname, vname, year) %>%
  group_by(runname, vname) %>%
  mutate(pch=value / lag(value) * 100 - 100) %>%
  summarise(vol=sd(pch, na.rm=TRUE)) %>%
  spread(vname, vol)



#****************************************************************************************************
#                determine tax levies ####
#****************************************************************************************************
# mvar is market value on the assessment roll - for now create it here
avroll_sum_pch <- avroll_sum %>%
  arrange(runname, year) %>%
  group_by(runname) %>%
  mutate_at(vars(-runname, -year, -sale), list(pch= ~. / lag(.) * 100 - 100))

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

