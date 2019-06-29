

#****************************************************************************************************
#                setup ####
#****************************************************************************************************
source("./programs/globals.r")
source("./programs/libraries.r")
source("./programs/functions.r")


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
#                get housing price data ####
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


#****************************************************************************************************
#                analysis ####
#****************************************************************************************************
glimpse(ptax)

df <- left_join(hpfy, ptax)

glimpse(df)

df2 <- df %>%
  filter(year>=2005, year<=2015)
df3 <-  left_join(df2 %>% filter(stabbr!="US"),
                  df2 %>% filter(stabbr=="US") %>% select(-stabbr) %>% rename(hpi_US=hpi, proptax_US=proptax)) %>%
  gather(variable, value, -stabbr, -year) %>%
  group_by(stabbr, variable) %>%
  mutate(ivalue = value / value[year==2007] * 100)


sts <- c("US", "CA", "FL", "NY", "NJ")
getplot <- function(ists, pnum){
  sts <- state.abb[ists]
  p <- df3 %>%
    filter(stabbr %in% sts) %>%
    ggplot(aes(year, ivalue, colour=variable, )) +
    geom_line(aes(linetype=variable, size=variable)) +
    scale_linetype_manual(values=c("solid", "dashed", "solid", "dashed")) +
    scale_size_manual(values=c(1.5, 1, 1.5, 1)) +
    scale_colour_manual(values=c("blue2", "blue2", "green2", "green2")) +
    geom_point() +
    geom_hline(yintercept = 100) +
    facet_wrap(~stabbr, ncol=4) +
    scale_y_continuous(breaks=seq(50, 200, 10)) +
    scale_x_continuous(breaks=seq(2005, 2025, 2))
  fname <- paste0()
  ggsave("./results/proptax_hpi.pdf", plot=p)
  
}

cut(1:length(state.abb), 4)



