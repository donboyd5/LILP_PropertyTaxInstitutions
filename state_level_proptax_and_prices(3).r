

#****************************************************************************************************
#                setup ####
#****************************************************************************************************
source("./subprograms/globals.r")
source("./subprograms/libraries.r")
source("./subprograms/functions.r")


#****************************************************************************************************
#                examine census property tax data ####
#****************************************************************************************************
glimpse(slgfin)
count(slgfin, aggvar)
ptax <- slgfin %>%
  filter(level==3, aggvar=="proptax") %>%
  select(stabbr, year, proptax=value)
count(ptax, year) %>% tail # through 2015

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



