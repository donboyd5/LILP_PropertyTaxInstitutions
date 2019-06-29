

#****************************************************************************************************
#                get assumptions ####
#****************************************************************************************************
# source(file.path(PROJHOME, "subprograms", "globals.r"))

#.. get market-value growth scenario ----
growthrates <- read_excel(globals$runcontrol,
                          sheet = "mv_growthrates",
                          range = cell_rows(c(5, NA)))
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
  ggplot(aes(year, mvg.factor, colour=growthratesname)) + 
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 1)


#.. get institutional rules and practices ----

# rules <- read_excel(paste0(rcdir, rcfn),
#                     sheet = "rules",
#                     range = cell_rows(c(4, NA)))
# rules <- rules %>%
#   filter(!is.na(rulesname)) %>%
#   select(-starts_with("X__"))
# rules

rules_avprocess <- read_excel(globals$runcontrol,
                              sheet = "rules_avprocess",
                              range = cell_rows(c(5, NA)))
rules_avprocess <- rules_avprocess %>%
  filter(!is.na(rulesname.av)) %>%
  select(-starts_with("...")) %>%
  mutate(tav.grcap=as.numeric(tav.grcap))
rules_avprocess

rules_levy <- read_excel(globals$runcontrol,
                              sheet = "rules_levy",
                              range = cell_rows(c(5, NA)))
rules_levy <- rules_levy %>%
  filter(!is.na(rulesname.levy)) %>%
  select(-starts_with("..."))
rules_levy

rules <- expand.grid(rulesname.av=unique(rules_avprocess$rulesname.av), 
                     rulesname.levy=unique(rules_levy$rulesname.levy),
                     stringsAsFactors = FALSE) %>%
  left_join(rules_avprocess) %>%
  left_join(rules_levy) %>%
  mutate(rulesname=paste0(rulesname.av, "-", rulesname.levy))
rules



#.. get tax base scenarios ----
taxbases <- read_excel(globals$runcontrol,
                       sheet = "taxbases",
                       range = cell_rows(c(5, NA)))

taxbases <- taxbases %>%
  filter(!is.na(taxbasename)) %>%
  group_by(taxbasename, ptype) %>%
  mutate(unit=row_number()) %>%
  ungroup
taxbases


#.. put run information together and create runset ----
runs <- read_excel(globals$runcontrol,
                   sheet = "runs",
                   range = cell_rows(c(5, NA))) %>%
  filter(!is.na(growthratesname)) %>%
  mutate(fullname=paste0(rulesname.av, "-", rulesname.levy, "-",
                         growthratesname, "-", taxbasename)) %>%
  select(runname, fullname, everything())
runs


runset <- 
  runs %>%
  left_join(growthrates_expanded) %>%
  left_join(rules) %>%
  left_join(taxbases) %>%
  arrange(fullname, ptype, unit, year) %>%
  select(runname, fullname, ptype, unit, year, everything())
glimpse(runset)

count(runset, runname, fullname)

