

#****************************************************************************************************
#                setup ####
#****************************************************************************************************
# source("./subprograms/globals.r")
# source("./subprograms/libraries.r")
# source("./subprograms/functions.r")
source(file.path(PROJHOME, "subprograms", "globals.r"))
source(file.path(PROJHOME, "subprograms", "libraries.r"))
source(file.path(PROJHOME, "subprograms", "functions.r"))

# library("forecast")
library("quantmod")
# library("viridis")
library("RColorBrewer")


#****************************************************************************************************
#                functions ####
#****************************************************************************************************
make_real <- function(value, year, baseyear){
  # value and year are two equal-length vectors
  # return a real vector indexed to the base year
  value * gdppi$gdppi[gdppi$year==baseyear] / gdppi$gdppi[match(year, gdppi$year)]
}


#****************************************************************************************************
#                Get previously stored data: hpi, ptax, gdppi, rgdp ####
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

# gdppi <- getSymbols("A191RG3A086NBEA", src='FRED', auto.assign = FALSE) %>%
#   as.data.frame() %>%
#   mutate(date=row.names(.) %>% as.Date, year=year(date)) %>%
#   select(year, gdppi=A191RG3A086NBEA)
# saveRDS(gdppi, "./data/gdppi.rds")
gdppi <- readRDS("./data/gdppi.rds")


# rgdp <- getSymbols("GDPCA", src='FRED', auto.assign = FALSE) %>%
#   as.data.frame() %>%
#   mutate(date=row.names(.) %>% as.Date, year=year(date)) %>%
#   select(year, rgdp=GDPCA)
# saveRDS(rgdp, "./data/rgdp.rds")
rgdp <- readRDS("./data/rgdp.rds")


ustax <- slgfin %>%
  filter(stabbr=="US", (level==3 & aggvar=="proptax") | (level==1 & aggvar %in% c("iit", "gst")), year %in% 1975:2016) %>%
  select(year, vname=aggvar, value)

ushpi <- hpfy %>%
  filter(stabbr=="US", year <= 2016) %>%
  mutate(vname="hpi") %>%
  select(year, vname, value=hpi)


#****************************************************************************************************
#                Graphs for report ####
#****************************************************************************************************
#.. U.S. recessions faceted time-series plots ----
# prepare data
pdata1 <- bind_rows(ustax, ushpi) %>%
  left_join(gdppi, by="year") %>%
  group_by(vname) %>%
  # mutate(rvalue=value * gdppi[year==2016] / gdppi) %>%
  mutate(rvalue=make_real(value, year, 2016)) %>%
  select(year, vname, rvalue) %>%
  bind_rows(rgdp %>% mutate(vname="rgdp") %>% select(year, vname, rvalue=rgdp))

# trend=loess(rvalue ~ year, span=.75)$fitted, detrend=rvalue - trend

# get peak(-x, +y) years indexed to peak year
ivalues <- function(vec, year, peak_year, minus, plus){
  # vec and year are vectors of same length
  # peak_year must be in year, as must peak_year - minus and peak_year + plus
  # NO ERROR CHECKING
  # returns a vector of length (peak_year - minus):(peak_year + plus)
  if(length(vec) != length(year)) print("ERROR!!!")
  # ystart <- year[peak_year - minus]
  # yend <- year[peak_year + plus]
  istart <- which(year==(peak_year - minus))
  iend <- which(year==(peak_year + plus))
  ivec <- vec / vec[year==peak_year] * 100
  ivec <- ivec[istart:iend]
  # df <- tibble(year=year[istart:iend], ivec=ivec)
  return(ivec)
}
# ivalues(1:20, 2001:2020, 2007, 3, 5)
tail(recessions) # let's use 1980, 1990, 2001, 2007
getrecs <- function(df, minus, plus){
  df2 <- tibble(iyear=-minus:plus,
                rec1980=ivalues(df$rvalue, df$year, 1980, minus, plus),
                rec1990=ivalues(df$rvalue, df$year, 1990, minus, plus),
                rec2001=ivalues(df$rvalue, df$year, 2001, minus, plus),
                rec2007=ivalues(df$rvalue, df$year, 2007, minus, plus))
  return(df2)
}
pdata2 <- pdata1 %>%
  group_by(vname) %>%
  do(getrecs(., 4, 5)) %>%
  gather(recyear, value, starts_with("rec")) %>%
  mutate(recyear=str_sub(recyear, 4, 7) %>% as.integer)
pdata2

capt1 <- "\nSources: "
capt2 <- "\nReal GDP and GDP price index (Bureau of Economic Analysis)"
capt3 <- "\nHousing prices (Federal Housing Finance Agency, https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx)"
capt4 <- "\nTaxes (Bureau of the Census, Annual State and Local Government Finance Survey)"
capt <- paste0(capt1, capt2, capt3, capt4)
gtitle <- "GDP, housing prices, and major tax revenue sources in recent recessions"
subt <- "All values adjusted for general price inflation"

pal_djb <- c("black", brewer.pal(n = 4, name = "Dark2")) # this is good

p <- pdata2 %>%
  filter(iyear >=-2) %>%
  mutate(longname=factor(vname, 
                         levels=c("rgdp", "hpi", "iit", "gst", "proptax"), 
                         labels=c("Real GDP", "Housing prices", "Individual income tax", "General sales tax", "Property taxes")),
         recname=paste0("Recession starting in ", recyear)) %>%
  group_by(longname) %>%
  ggplot(aes(iyear, value, colour=longname, linetype=longname)) +
  geom_line(size=1.2) +
  # geom_point() +
  geom_hline(yintercept = 100, linetype="dashed") +
  geom_vline(xintercept = 0, linetype="dotted") +
  scale_x_continuous(name="Year (Recession start=0)", breaks=-10:10) +
  scale_y_continuous(name="Index (Recession start=100)", breaks=seq(-100, 300, 20), limits=c(NA, NA)) +
  # scale_colour_manual(values=c("blue", "red", "forestgreen", "cyan", "purple")) +
  # scale_color_viridis(discrete = TRUE, option = "E") +
  # scale_color_brewer(palette = "Set1") +
  scale_colour_manual(values=pal_djb) +
  scale_linetype_manual(values=c(rep("solid", 4), "dashed")) + 
  ggtitle(gtitle, subtitle=subt) +
  facet_wrap(~recname, ncol=2) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  labs(caption=capt) +
  theme(plot.caption = element_text(hjust=0, size=8))
p
ggsave(paste0("./results/", "US_longterm_hpi_ptax.png"), p, scale=1.3, width=7, height=6, units="in")


#.. Line graph case-study states and US ----
# levs <- c("US", "CA", "FL", "MA", "NH", "OH")
# levs <- c("US", "CA", "FL", "MA", "NH", "RI")
levs <- c("US", "CA", "FL", "MA", "NH", "PA")
labs <- c("United States", state.name)[match(levs, c("US", state.abb))]
cbind(levs, labs)
capt1 <- "\nSources: "
capt2 <- "\nGDP price index (Bureau of Economic Analysis)"
capt3 <- "\nHousing prices (Federal Housing Finance Agency, https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx)"
capt4 <- "\nProperty taxes (Bureau of the Census, Annual State and Local Government Finance Survey)"
capt <- paste0(capt1, capt2, capt3, capt4)
gtitle <- "Housing prices and property tax revenue in the U.S. and in case-study states"
subt <- "All values adjusted for general price inflation"

p <- hpi_ptax %>%
  filter(year %in% 2000:2016, stabbr %in% levs) %>%
  mutate(value=value * gdppi$gdppi[gdppi$year==2016] / gdppi$gdppi[match(year, gdppi$year)]) %>%
  mutate(stname=factor(stabbr, levels=levs, labels=labs),
         longname=factor(vname, 
                         levels=c("hpi", "proptax"), 
                         labels=c("Housing prices", "Property taxes"))) %>%
  group_by(stname, vname, longname) %>%
  arrange(year) %>%
  # interpolate property tax values for the missing years
  mutate(value=ifelse(vname=="proptax" & year %in% c(2001, 2003),
                      (value[match(year + 1, year)] + value[match(year - 1, year)]) / 2,
                      value)) %>%
  mutate(ivalue=value / value[year==2007] * 100) %>%
  filter(year %in% 2002:2016) %>%
  ggplot(aes(year, ivalue, colour=longname)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 100, linetype="dashed") +
  geom_vline(xintercept = 2007, linetype="dotted") +
  scale_x_continuous(name=NULL, breaks=seq(2000, 2030, 2)) +
  scale_y_continuous(name="Index (2007=100)", breaks=seq(-100, 300, 20), limits=c(NA, NA)) +
  scale_colour_manual(values=c("blue", "red")) +
  ggtitle(gtitle, subtitle=subt) +
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
get_peakyear <- function(var, year, years){
  year[year %in% years][which.max(var[year %in% years])]
}
get_troughyear <- function(var, year, years, peakyear){
  # trough year MUST be later than peak year
  # redefine year so that it starts in the peak year
  years <- max(years[1], peakyear):max(years)
  year[year %in% years][which.min(var[year %in% years])]
}

all_years <- 2004:2015

hpi_peakyears <- 2005:2009
hpi_troughyears <- 2009:2012

pt_peakyears <- 2005:2010
pt_troughyears <- 2009:2014

peaktrough <- hpi_ptax %>%
  filter(year %in% all_years) %>%
  mutate(value=make_real(value, year, 2016)) %>%
  spread(vname, value) %>%
  group_by(stabbr) %>%
  summarise(hpi_peak=get_peakyear(hpi, year, hpi_peakyears),
            hpi_trough=get_troughyear(hpi, year, hpi_troughyears, hpi_peak),
            rpt_peak=get_peakyear(proptax, year, pt_peakyears),
            rpt_trough=get_troughyear(proptax, year, pt_troughyears, rpt_peak),
            
            hpi_peak_val=hpi[year==hpi_peak[1]],
            hpi_trough_val=hpi[year==hpi_trough[1]],
            rpt_peak_val=proptax[year==rpt_peak[1]],
            rpt_trough_val=proptax[year==rpt_trough[1]]) 

# %>%  mutate(hpi_trough_val=ifelse(hpi_trough < hpi_peak, NA, hpi_trough_val),    rpt_trough_val=ifelse(rpt_trough < rpt_peak, NA, rpt_trough_val))

peaktrough %>% filter(stabbr=="AL")

ptchange <- peaktrough %>%
  mutate(hpi_pch=hpi_trough_val / hpi_peak_val * 100 - 100,
         rpt_pch=rpt_trough_val / rpt_peak_val * 100 - 100,
         diff=rpt_pch - hpi_pch,
         rpt_peaklag=rpt_peak - hpi_peak,
         rpt_troughlag=rpt_trough - hpi_trough)

ptchange %>%
  arrange(diff) %>%
  write_csv("./results/ptresilience.csv")

ptchange %>%
  arrange(diff) %>%
  ht(10)

ptchange %>%
  arrange(rpt_peaklag) %>%
  ht(10)


#.. Scatterplot with the peak-trough changes ----
pdata <- ptchange %>%
  filter(stabbr != "DC") %>%
  mutate(css=case_when(stabbr %in% globals$css ~ "css",
                       stabbr !="US" ~ "other",
                       TRUE ~ "US"),
         css=factor(css, levels=c("css", "other", "US"), labels=c("Case study state", "Other states", "US average")))

# lims <- c(NA, NA)
capt1 <- "\nSources: Housing prices (Federal Housing Finance Agency,\nhttps://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx)"
capt2 <- "\nProperty tax (Bureau of the Census, Annual State and Local Government Finance Survey)"
capt <- paste0(capt1, capt2)

capt1 <- "\nSources: "
capt2 <- "\nGDP price index (Bureau of Economic Analysis)"
capt3 <- "\nHousing prices (Federal Housing Finance Agency, https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx)"
capt4 <- "\nProperty taxes (Bureau of the Census, Annual State and Local Government Finance Survey)"
capt <- paste0(capt1, capt2, capt3, capt4)

ylims <- c(-37, 0)
xlims <- c(-60, 1)
p <- pdata %>%
  ggplot(aes(x=hpi_pch, y=rpt_pch, label=stabbr, colour=css)) +
  geom_text_repel(size=3.2, min.segment.length=0.7) +
  # geom_text(size=3.2) +
  geom_point(size=0.9) +
  scale_colour_manual(values=c("blue", "darkgreen", "black")) +
  scale_x_continuous(name="% change in housing prices, peak to trough", breaks=seq(-100, 100, 5), limits=xlims) +
  scale_y_continuous(name="% change in property tax revenue, peak to trough", breaks=seq(-100, 100, 5), limits=ylims) +
  geom_hline(yintercept = median(ptchange$rpt_pch[ptchange$stabbr!="US"]), linetype="dashed") +
  geom_vline(xintercept = median(ptchange$hpi_pch[ptchange$stabbr!="US"]), linetype="dashed") +
  geom_hline(yintercept = 0, linetype="solid", size=.35) +
  geom_abline(slope=1, intercept=0) +
  ggtitle(label="Peak to trough change in inflation-adjusted housing prices and property tax revenue",
          subtitle="Dashed lines show medians") +
  annotate("text", x = -3, y = -22, label = "Below diagonal:\nProperty tax\nfell by more than\nhousing prices", size=3.4) +
  annotate("text", x = -32, y = -22, label = "Above diagonal:\nProperty tax\nfell by less than\nhousing prices", size=3.4) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  labs(caption=capt) +
  theme(plot.caption = element_text(hjust=0, size=8))
p
ggsave(paste0("./results/hpi_proptax_scatter.png"), p, scale=1.75, width=6, height=6, units="in")



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
#               Analytic graph: 50-states time series plot ####
#****************************************************************************************************
# hpfy <- readRDS("./data/hpfy.rds")
# glimpse(ptax)
# df <- left_join(hpfy, ptax)

glimpse(hpi_ptax)
# put the us on every record

pdata1 <- hpi_ptax %>%
  filter(year>=2003, year<=2016) %>%
  group_by(stabbr, vname) %>%
  mutate(value=make_real(value, year, 2016)) %>%
  ungroup
pdata <- pdata1 %>%
  filter(stabbr!="US") %>%
  left_join(pdata1 %>% 
              filter(stabbr=="US") %>%
              select(year, vname, value_US=value),
            by = c("year", "vname")) %>%
  gather(valtype, value, -stabbr, -year, -vname) %>%
  ungroup %>%
  mutate(vname=ifelse(valtype=="value_US", paste0(vname, "_US"), vname)) %>%
  select(-valtype) %>%
  group_by(stabbr, vname) %>%
  mutate(ivalue = value / value[year==2007] * 100) %>%
  ungroup

tmp <- pdata %>% filter(stabbr=="US") %>% arrange(year, vname)

sts <- c("US", "CA", "FL", "NY", "NJ")
getplot <- function(sts, pnum, nrow, ncol){
  gtype <- paste0("_", nrow, "x", ncol, "_")
  p <- pdata %>%
    filter(stabbr %in% sts) %>%
    ggplot(aes(year, ivalue, colour=vname)) +
    geom_line(aes(linetype=vname, size=vname)) +
    scale_linetype_manual(values=c("solid", "dashed", "solid", "dashed")) +
    scale_size_manual(values=c(1.3, .8, 1.3, .8)) +
    scale_colour_manual(values=c("blue2", "blue2", "red", "red")) +
    geom_point() +
    geom_hline(yintercept = 100) +
    scale_y_continuous(name="Indexed to 2007=100", breaks=seq(50, 200, 10), limits=c(40, 160)) +
    scale_x_continuous(name=NULL, breaks=seq(2000, 2025, 2)) +
    theme(axis.text.x=element_text(angle=-45, hjust=0,vjust=1,size=10)) +
    facet_wrap(~stabbr, nrow=nrow, ncol=ncol) +
    ggtitle("Inflation-adjusted housing prices and local property tax by state, plus U.S. values, indexed to 2007=100",
            subtitle="Solid lines are the state, dashed are U.S. Blue is housing values, red is property tax")
  fname <- paste0("./results/proptax_hpi2", gtype, pnum, ".png")
  ggsave(fname, plot=p, width = 10, height = 9, units="in")
}

# getplot(state.abb[1:12], 1, 3, 3)
getplot(sts, 1, 3, 3)

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
#                OLD BELOW HERE examine census property tax data ####
#****************************************************************************************************

