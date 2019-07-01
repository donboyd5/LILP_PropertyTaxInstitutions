

#****************************************************************************************************
#                setup ####
#****************************************************************************************************
# source("./subprograms/globals.r")
# source("./subprograms/libraries.r")
# source("./subprograms/functions.r")
source(file.path(PROJHOME, "subprograms", "globals.r"))
source(file.path(PROJHOME, "subprograms", "libraries.r"))
source(file.path(PROJHOME, "subprograms", "functions.r"))


#****************************************************************************************************
#                case study states hpi and ptax ####
#****************************************************************************************************
hpfy <- readRDS("./data/hpfy.rds")
glimpse(hpfy)
count(hpfy, stabbr)

ptax <- slgfin %>%
  filter(level==3, aggvar=="proptax") %>%
  select(stabbr, year, proptax=value)
glimpse(ptax)

hpi_ptax <- left_join(hpfy, ptax) %>%
  gather(vname, value, hpi, proptax)
glimpse(hpi_ptax)


levs <- c("US", "CA", "FL", "MA", "NH", "OH")
labs <- c("United States", state.name)[match(levs, c("US", state.abb))]
cbind(levs, labs)
capt1 <- "\nSources: Housing prices (Federal Housing Finance Agency, https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx)"
capt2 <- "\nProperty tax (Bureau of the Census, Annual State and Local Government Finance Survey)"
capt3 <- "\nNote: Property tax values for 2001 and 2003 were missing, and were constructed by interpolation"
capt <- paste0(capt1, capt2, capt3)
gtitle <- "Housing prices and property tax revenue in the U.S. and in case-study states"

p <- hpi_ptax %>%
  filter(year %in% 2000:2016, stabbr %in% levs) %>%
  mutate(stname=factor(stabbr, levels=levs, labels=labs),
         longname=factor(vname, 
                         levels=c("hpi", "proptax"), 
                         labels=c("Housing prices", "Property taxes"))) %>%
  group_by(stname, vname, longname) %>%
  arrange(year) %>%
  # interpolate property tax values
  mutate(value=ifelse(vname=="proptax" & year %in% c(2001, 2003),
                      (value[match(year + 1, year)] + value[match(year - 1, year)]) / 2,
                      value)) %>%
  mutate(ivalue=value / value[year==2007] * 100) %>%
  ggplot(aes(year, ivalue, colour=longname)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 100, linetype="dashed") +
  geom_vline(xintercept = 2007, linetype="dotted") +
  scale_x_continuous(name=NULL, breaks=seq(2000, 2030, 2)) +
  scale_y_continuous(name="Index (2007=100)", breaks=seq(-100, 300, 20), limits=c(NA, NA)) +
  scale_colour_manual(values=c("blue", "red")) +
  ggtitle(gtitle) +
  facet_wrap(~stname, ncol=2) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  labs(caption=capt) +
  theme(plot.caption = element_text(hjust=0, size=8))
p
ggsave(paste0("./results/", "css_hpi_ptax.png"), p, scale=1.3, width=7, height=6, units="in")


#****************************************************************************************************
#                resilience ####
#****************************************************************************************************
glimpse(hpi_ptax)
count(hpi_ptax, vname)

# determine each state's own hpi peak and trough, then search for its pt peak and trough between
# x years before and y years after
getmaxyear <- function(var, year, years){
  year[year %in% years][which.max(var[year %in% years])]
}
getminyear <- function(var, year, years){
  year[year %in% years][which.min(var[year %in% years])]
}

all_years <- 2004:2015

hpi_peakyears <- 2005:2009
pt_peakyears <- 2005:2010

hpi_troughyears <- 2009:2012
pt_troughyears <- 2009:2014

peaktrough <- hpi_ptax %>%
  filter(year %in% all_years) %>%
  spread(vname, value) %>%
  group_by(stabbr) %>%
  summarise(hpi_peak=getmaxyear(hpi, year, hpi_peakyears),
         rpt_peak=getmaxyear(proptax, year, pt_peakyears),
         hpi_trough=getminyear(hpi, year, hpi_troughyears),
         rpt_trough=getminyear(proptax, year, pt_troughyears),
         hpi_peak_val=hpi[year==hpi_peak[1]],
         hpi_trough_val=hpi[year==hpi_trough[1]],
         rpt_peak_val=proptax[year==rpt_peak[1]],
         rpt_trough_val=proptax[year==rpt_trough[1]])
peaktrough %>% filter(stabbr=="AL")

ptchange <- peaktrough %>%
  mutate(hpi_pch=hpi_trough_val / hpi_peak_val * 100 - 100,
         rpt_pch=rpt_trough_val / rpt_peak_val * 100 - 100,
         diff=rpt_pch - hpi_pch)

ptchange %>%
  arrange(diff) %>%
  ht(10)

ptchange %>%
  arrange(diff) %>%
  write_csv("./results/ptresilience.csv")

#.. Scatterplot with the peak-trough changes ----
pdata <- ptchange %>%
  filter(stabbr != "DC") %>%
  mutate(css=case_when(stabbr %in% globals$css ~ "css",
                       stabbr !="US" ~ "other",
                       TRUE ~ "US"),
         css=factor(css, levels=c("css", "other", "US"), labels=c("Case study state", "Other states", "US average")))

# lims <- c(NA, NA)
capt1 <- "\nSources: Housing prices (Federal Housing Finance Agency, https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx)"
capt2 <- "\nProperty tax (Bureau of the Census, Annual State and Local Government Finance Survey)"
capt <- paste0(capt1, capt2)
ylims <- c(-29, 0)
xlims <- c(-60, 1)
p <- pdata %>%
  ggplot(aes(x=hpi_pch, y=rpt_pch, label=stabbr, colour=css)) +
  geom_text_repel(size=3.2, min.segment.length=0.7) +
  scale_colour_manual(values=c("blue", "darkgreen", "black")) +
  scale_x_continuous(name="% change in housing prices, peak to trough", breaks=seq(-100, 100, 5), limits=xlims) +
  scale_y_continuous(name="% change in property tax revenue, peak to trough", breaks=seq(-100, 100, 5), limits=ylims) +
  geom_hline(yintercept = median(ptchange$rpt_pch[ptchange$stabbr!="US"]), linetype="dashed") +
  geom_vline(xintercept = median(ptchange$hpi_pch[ptchange$stabbr!="US"]), linetype="dashed") +
  geom_abline(slope=1, intercept=0) +
  ggtitle(label="Peak to trough change in housing prices and property tax revenue",
          subtitle="Dashed lines show medians") +
  annotate("text", x = -3, y = -28, label = "Below diagonal:\nProperty tax\nfell by more than\nhousing prices", size=3.4) +
  annotate("text", x = -29, y = -14, label = "Above diagonal:\nProperty tax\nfell by less than\nhousing prices", size=3.4) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  labs(caption=capt) +
  theme(plot.caption = element_text(hjust=0, size=8))
p
ggsave(paste0("./results/hpi_proptax_scatter.png"), p, scale=1.3, width=6, height=6, units="in")
#   geom_text_repel(min.segment.length=0.5)




resilience <- hpi_ptax %>%
  filter(year %in% all_years) %>%
  spread(vname, value) %>%
  right_join(peaktrough) %>%
  group_by(stabbr) %>%
  mutate(hpi_peak_val=hpi[year==hpi_peak[1]],
         hpi_trough_val=hpi[year==hpi_trough[1]],
         rpt_peak_val=proptax[year==rpt_peak[1]],
         rpt_trough_val=proptax[year==rpt_trough[1]],)
resilience



#****************************************************************************************************
#                examine census property tax data ####
#****************************************************************************************************
glimpse(slgfin)
count(slgfin, aggvar)
ptax <- slgfin %>%
  filter(level==3, aggvar=="proptax") %>%
  select(stabbr, year, proptax=value)
count(ptax, year) %>% tail # through 2016

ptax %>%
  filter(year >=2006, stabbr %in% c("US", "NY", "CA", "FL", "NV")) %>%
  group_by(stabbr) %>%
  arrange(year) %>%
  mutate(iproptax=proptax / proptax[year==2008] * 100) %>%
  ggplot(aes(year, iproptax, colour=stabbr)) +
  geom_line() +
  geom_point() + 
  geom_hline(yintercept=100) +
  scale_x_continuous(breaks=2000:2020)
  

#****************************************************************************************************
#                ONETIME -- get housing price data ####
#****************************************************************************************************
# https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx
# https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_us_and_census.csv

url.base <- "https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/"

us.fn <- "HPI_AT_us_and_census.csv"
download.file(paste0(url.base, us.fn), paste0("./data/", us.fn), mode="wb")


# state all-transactions indexes
s.fn <- "HPI_AT_state.csv"
download.file(paste0(url.base, s.fn), paste0("./data/", s.fn), mode="wb")

#.. read the data ----
us.hp <- read_csv(paste0("./data/", us.fn), col_names=c("stabbr", "year", "qtr", "hpi"), col_types="ciid") %>%
  filter(stabbr=="USA") %>%
  mutate(stabbr="US")
ht(us.hp)

s.hp <- read_csv(paste0("./data/", s.fn), col_names=c("stabbr", "year", "qtr", "hpi"), col_types="ciid")

hpq <- bind_rows(us.hp, s.hp) %>%
  mutate(date=yq(paste(year, qtr)))
# yq("2010 4")
glimpse(hpq)
saveRDS(hpq, "./data/hpq.rds")

hpfy <- 
  bind_rows(us.hp, s.hp) %>%
  mutate(fy=ifelse(qtr > 2, year + 1, year)) %>%
  group_by(stabbr, fy) %>%
  summarise(hpi=mean(hpi)) %>%
  ungroup %>%
  rename(year=fy)
ht(hpfy)
count(hpfy, year)
count(hpfy, stabbr)
saveRDS(hpfy, "./data/hpfy.rds")

#.. get the msa data ----
# https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_metro.csv
m.fn <- "https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_metro.csv"
m.hp <- read_csv(paste0(m.fn), col_names=c("msaname", "msaid", "year", "qtr", "hpi", "se"), col_types="cciidd")
# "Abilene, TX",10180,1986,3,107.67,-2.75
ht(m.hp)
m.hp2 <- m.hp %>%
  filter(!is.na(hpi))
saveRDS(m.hp2, "./data/msa_hpq.rds")

# look at San Mateo
m.hp2 <- readRDS("./data/msa_hpq.rds")

m.hp2 %>% 
  filter(str_detect(msaname, "San Fran"), qtr==1) %>%
  arrange(year) %>%
  mutate(pchya=hpi / lag(hpi) * 100 - 100) %>%
  write_csv("./data/sanmateo_msa_hpi.csv")

# 41884






#****************************************************************************************************
#                q pchya ####
#****************************************************************************************************
hpq <- readRDS("./data/hpq.rds")
glimpse(hpq)

# US peak was 2007q1
hpq %>%
  filter(stabbr=="US", year >= 2004) %>%
  as.data.frame()

hpq %>%
  filter(stabbr %in% c("US", globals$css), year >= 2000) %>%
  group_by(stabbr) %>%
  mutate(ihpi=hpi / hpi[date=="2007-01-01"] * 100) %>%
  ggplot(aes(date, ihpi, colour=stabbr)) +
  geom_line() + geom_point() +
  scale_x_date(date_breaks = "2 years", date_labels="%Y")

hpqpch <- hpq %>%
  group_by(stabbr) %>%
  mutate(pch=hpi / hpi[match(date - months(12), date)] * 100 - 100) %>%
  ungroup
# ht(hpqpch)

hpqpch %>%
  filter(stabbr=="US", qtr==1, year >= 2000)

# save the hpqpch to a file as the great recession
hpqpch %>%
  filter(year >= 2000, qtr==1) %>%
  select(-qtr, -hpi) %>%
  spread(stabbr, pch) %>%
  write_csv("./data/greatrec_states.csv")

write_csv(hpqpch, "./data/hqppch.csv")


#****************************************************************************************************
#                analysis ####
#****************************************************************************************************
hpfy <- readRDS("./data/hpfy.rds")
glimpse(ptax)

df <- left_join(hpfy, ptax)

glimpse(df)

df2 <- df %>%
  filter(year>=2003, year<=2015)

df3 <-  left_join(df2 %>% filter(stabbr!="US"),
                  df2 %>% filter(stabbr=="US") %>% select(-stabbr) %>% rename(hpi_US=hpi, proptax_US=proptax)) %>%
  gather(variable, value, -stabbr, -year) %>%
  group_by(stabbr, variable) %>%
  mutate(ivalue = value / value[year==2007] * 100)


sts <- c("US", "CA", "FL", "NY", "NJ")
sts <- c("US", "CA", "FL", "NY", "NJ")
getplot <- function(sts, pnum, nrow, ncol){
  gtype <- paste0("_", nrow, "x", ncol, "_")
  p <- df3 %>%
    filter(stabbr %in% sts) %>%
    ggplot(aes(year, ivalue, colour=variable, )) +
    geom_line(aes(linetype=variable, size=variable)) +
    scale_linetype_manual(values=c("solid", "dashed", "solid", "dashed")) +
    scale_size_manual(values=c(1.3, .8, 1.3, .8)) +
    scale_colour_manual(values=c("blue2", "blue2", "red", "red")) +
    geom_point() +
    geom_hline(yintercept = 100) +
    scale_y_continuous(name="Indexed to 2007=100", breaks=seq(50, 200, 10), limits=c(40, 160)) +
    scale_x_continuous(name=NULL, breaks=seq(2000, 2025, 2)) +
    theme(axis.text.x=element_text(angle=-45, hjust=0,vjust=1,size=10)) +
    facet_wrap(~stabbr, nrow=nrow, ncol=ncol) +
    ggtitle("Housing prices and local property tax by state, plus U.S. values, indexed to 2007=100",
            subtitle="Solid lines are the state, dashed are U.S. Blue is housing values, red is property tax")
  fname <- paste0("./results/proptax_hpi", gtype, pnum, ".png")
  ggsave(fname, plot=p, width = 10, height = 9, units="in")
}

getplot(state.abb[1:12], 1)

start <- 0
end <- 0
nrow <- 3
ncol <- 3
npanels <- nrow * ncol
for(pnum in 1:6){
  start <- end + 1
  end <- pmin(start + npanels - 1, 50)
  print(paste0("States: ", paste(state.abb[start:end], collapse=" ")))
  sts <- state.abb[start:end]
  getplot(sts, pnum, nrow, ncol)
}


#****************************************************************************************************
#                case-study states ####
#****************************************************************************************************

# list of states in my table
tablist <- c("AZ", "CA", "FL", "MD", "NV",
             "NJ", "NY", "RI", "SC", "UT",
             "VA", "WA")

# sts <- c("AZ", "CA", "RI", "NY", "NJ")
stn <- c(1, 2, 4, 5, 8, 10, 11, 12)
sts <- tablist[stn]
sts

sts <- c(sts, "TX") %>% sort

# sts <- c("CA", "MD", "NY", "RI", "VA")

sts <- c("CA", "FL", "MA", "NH", "IL", "NY")

sts <- c("CA", "FL", "MA", "NH")

# keep: MD

p.cs <- df3 %>%
  ungroup %>%
  filter(stabbr %in% sts) %>%
  mutate(stabbr=factor(stabbr, levels=sts)) %>%
  ggplot(aes(year, ivalue, colour=variable, )) +
  geom_line(aes(linetype=variable, size=variable)) +
  scale_linetype_manual(values=c("solid", "dashed", "solid", "dashed")) +
  scale_size_manual(values=c(1.3, .8, 1.3, .8)) +
  scale_colour_manual(values=c("blue2", "blue2", "red", "red")) +
  geom_point() +
  geom_hline(yintercept = 100) +
  scale_y_continuous(name="Indexed to 2007=100", breaks=seq(50, 200, 10), limits=c(40, 160)) +
  scale_x_continuous(name=NULL, breaks=seq(2000, 2025, 2)) +
  theme(axis.text.x=element_text(angle=-45, hjust=0,vjust=1,size=10)) +
  facet_wrap(~stabbr, ncol=2) +
  ggtitle("Housing prices and local property tax by state, plus U.S. values, indexed to 2007=100",
          subtitle="Solid lines are the state, dashed are U.S. Blue is housing values, red is property tax")
p.cs



