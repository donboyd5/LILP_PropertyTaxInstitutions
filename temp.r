prop_matrices %>% filter(is.na(propid))

rcuse

rcuse %>%
  mutate(nprop_avcycle=salecycle_nyears * avcycle_nyears,
         nprop2=ifelse(avcycle_salemethod != "deterministic", max(nprop_avcycle, avcycle_nprop), nprop_avcycle)) %>%
  select(runname, avcycle_salemethod, nprop_avcycle, nprop2, salecycle_nyears, avcycle_nyears) # I don't want 10k props for cycle as it is 0%

rcuse %>%
  mutate(nprop_acq=salecycle_nyears,
         nprop2=ifelse(acqvalue_salemethod != "deterministic", max(nprop_acq, acqvalue_nprop), nprop_acq)) %>%
  select(runname, acqvalue_salemethod, nprop_acq, nprop2, salecycle_nyears, acqvalue_nprop)


glimpse(avroll)

avroll %>%
  group_by(runname) %>%
  summarise(n=n())

avroll %>%
  filter(ptype=="acquisition") %>%
  group_by(runname, year) %>%
  summarise(sale=mean(av_acquisition_sale_year)) %>%
  spread(runname, sale)

avroll %>%
  filter(ptype=="acquisition") %>%
  group_by(runname) %>%
  summarise(n=n(), sale=mean(av_acquisition_sale_year))


tmp <- avroll %>%
  filter(ptype=="acquisition") %>%
  select(runname, propid, year, index, av_acquisition_sale_year)

tmp %>%
  group_by(runname) %>%
  summarise(n=n())

tmp %>% write_csv("d:/temp/tmp2.csv")


pids <- c(1, 101, 201, 301)
pids <- c(1, 2, 3, 4)
tmp %>%
  filter(runname=="CA_rules_CAGR_20ysale_binomial10k", propid %in% pids) %>%
  spread(propid, av_acquisition_sale_year)




rbinom(n, size, prob) # (nobs, ntrials, prob)
rbinom(n, size, prob)

sample(0:1, 10, replace=TRUE, prob=c(.5, .5))

rbinom(10, 1, .5)

rbinom(nprop * nyears, 1, 1 / 20)


nprop <- 100
npropk <- 1000
nyears <- 61
m1 <- matrix(NA, nrow=nprop, ncol=nyears)
m1k <- matrix(NA, nrow=npropk, ncol=nyears)

set.seed(1234); vals <- rbinom(nprop * nyears, 1, 1 / 20)
set.seed(1234); valsk <- rbinom(npropk * nyears, 1, 1 / 20)

m2 <- get_mat(m1, vals)
m2k <- get_mat(m1k, valsk)

rows <- c(1:10)
cols <- c(1:20)
m2[rows, cols]

rowsk <- c(1:10, 101:110)
cols <- c(1:20)
m2k[rowsk, cols]

rbinom(nprop * nyears, 1, 1 / avg_years_between_sale)

dim(mv_list$m_mvtrue_acquisition)
dim(mv_list$m_mvtrue)

