

#****************************************************************************************************
#                setup ####
#****************************************************************************************************
source("./subprograms/globals.r")
source("./subprograms/libraries.r")
source("./subprograms/functions.r")


#****************************************************************************************************
#                run the model ####
#****************************************************************************************************
source("./subprograms/get_assumptions.r")

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
  summarise_at(vars(mv.on.roll, av, av.taxable), funs(sum(. * weight))) %>%
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
  summarise_at(vars(mv.true, mv.known, mv.on.roll, av, av.taxable, levy), funs(sum(. * weight))) %>%
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
  # select(fullname, year, assess.cycle, initial.reassess.year, reassess.year, adjust.period, mv.true, mv.known, mv.on.roll)

  




write_xlsx(tmp, "./results/tmp.xlsx", col_names = TRUE)
