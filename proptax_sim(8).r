

#****************************************************************************************************
#                setup ####
#****************************************************************************************************
source(file.path(PROJHOME, "subprograms", "globals.r"))
source(file.path(PROJHOME, "subprograms", "libraries.r"))
source(file.path(PROJHOME, "subprograms", "functions.r"))



#****************************************************************************************************
#                test area ####
#****************************************************************************************************
nyears <- 50
burnyears <- 11
totyears <- nyears + burnyears
(years <- 1:totyears)

assume <- list()
assume$avcycle <- 3
assume$avcycle_startyear <- 1
assume$armv_lag <- 2
assume$salecycle <- 8
assume$salecycle_startyear <- 9
assume$max_mvpch	<- .1
assume$av_ratio <- 1
assume$av_grcap <- .02

init <- list()
init$mv	<- 100

# vectors

# the Great Recession shock, approximately - MV growth rates for US as a whole
gr_shock_us <- c(.06, .1, .1, .03, -.02, -.06, -.07, -.03, -.01, .02, .05, .05)
mv_gr <- c(rep(.06, burnyears), 
           gr_shock_us,
           rep(.06, totyears - burnyears -length(gr_shock_us)))

# get reassessment cycle years
get_cycle <- function(totyears, burnyears, cycle, truestart, burn=TRUE){
  # return vector with length totyears and
  # 1 in the years in which there is a reassessment, 
  # starting in the right place
  istartyear <- burnyears + truestart
  
  if(burn) burnvals <- seq(istartyear - cycle, 1, -cycle) %>% rev() else { # first part of sequence - step down to 1
    if(burn==FALSE) burnvals <- NULL}

  cycleyears <- c(burnvals,
                  seq(istartyear, totyears, cycle)) # second part of sequence - step up to final value

  vcycle <- rep(0, totyears)
  vcycle[cycleyears] <- 1
  return(vcycle)
}

reassess <- get_cycle(totyears, burnyears, assume$avcycle, assume$avcycle_startyear)
sale <- get_cycle(totyears, burnyears, assume$avcycle, assume$avcycle_startyear, burn=FALSE)

df <- tibble(index=1:totyears,
             year=-(burnyears - 1):nyears,
             mv_gr=mv_gr,
             mv=100 * cumprod(1 + c(0, mv_gr[-totyears])),
             reassess=get_cycle(totyears, burnyears, assume$avcycle, assume$avcycle_startyear),
             sale=get_cycle(totyears, burnyears, assume$salecycle, assume$salecycle_startyear, burn=FALSE))
df

mv <- df$mv; sale=df$sale

get_mv_acquisition <- function(mv, sale, av_grcap){
  mv_acquisition <- rep(0, length(mv))
  mv_acquisition[1] <- mv[1]
  for(i in 2:length(mv)) {
    mv_acquisition[i] <- ifelse(sale[i]==1, 
                                mv[i], 
                                min(mv[i], mv_acquisition[i - 1] * (1 +  av_grcap)))
  }
  return(mv_acquisition)
}
get_mv_acquisition(df$mv, df$sale, assume$av_grcap)

get_mv_cycle <- function(mv, reassess, avcycle_startyear, armv_lag){
  mv_cycle <- rep(0, length(mv))
  
  mv_cycle[1:armv_lag] <- mv[1]
  for(i in ((armv_lag + 1):length(mv))){
    mv_cycle[i] <- ifelse(reassess[i] == 1,
                         mv[i - armv_lag],
                         mv_cycle[i - 1])
  }
  
  return(mv_cycle)
}
# get_mv_cycle(df$mv, df$reassess, assume$avcycle_startyear, assume$armv_lag)


df2 <- df %>%
  mutate(mv_acquisition=get_mv_acquisition(mv, sale, assume$av_grcap),
         mv_avcycle=get_mv_cycle(mv, reassess, assume$avcycle_startyear, assume$armv_lag))
# next add taxes, then larger datasets
df2 %>% data.frame

df2 %>%
  pivot_longer(cols=(c(mv, mv_acquisition, mv_avcycle)),
               names_to = "name", 
               values_to="value") %>%
  filter(year>=1, year<=25) %>%
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(0, 500, 50)) +
  theme_bw()

df2 %>%
  pivot_longer(cols=(c(mv, mv_acquisition, mv_avcycle)),
               names_to = "name", 
               values_to="value") %>%
  filter(year>=1, year<=25) %>%
  group_by(name) %>%
  mutate(ivalue=value / value[year==1] * 100) %>%
  ggplot(aes(year, ivalue, colour=name)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(0, 500, 50)) +
  scale_color_manual(values=c("blue", "#fd8d3c", "#fed976")) +
  theme_bw()





#****************************************************************************************************
#                run the model ####
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
