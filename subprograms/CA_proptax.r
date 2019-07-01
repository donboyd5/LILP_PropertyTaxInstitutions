

glimpse(ca_ptax)
glimpse(ca_avalues)
glimpse(ca_hpi)

toptax <- ca_ptax2 %>%
  filter(fyear==2019, vname=="ptax_tot") %>%
  top_n(6, value)
toptax

count(ca_ptax, county)
count(ca_avalues, county)
count(ca_hpi, msaid, msaname)

# Alameda San Francisco-Oakland-Hayward, CA MSA
# LA 31084 Los Angeles-Long Beach-Glendale, CA  (MSAD)

# https://en.wikipedia.org/wiki/California_statistical_areas
# link the 3 files
link <- read_csv("cnty_ptax, cnty_av, cnty_msa
Alameda, Alameda, 41884
Los Angeles, Los Angeles, 31084
Orange, Orange, 31084
Riverside, Riverside, 40140
San Diego, San Diego, 41740
Santa Clara, Santa Clara, 41940") %>%
  mutate(cnty_msa=as.character(cnty_msa))
link

ca_all <- bind_rows(link %>% select(county=cnty_ptax) %>% 
                      left_join(ca_ptax2, by="county"),
                    link %>% select(county=cnty_av) %>%
                      left_join(ca_avalues %>% mutate(vname="avtot") %>% rename(value=avtot) %>% mutate(fyear=year1 + 1) %>% select(-year1),
                                by="county"),
                    link %>% select(county=cnty_av, msaid=cnty_msa) %>% 
                      left_join(ca_hpi %>% filter(qtr==1) %>% select(msaid, msaname, fyear=year, hpi) %>% mutate(vname="hpi") %>% rename(value=hpi),
                                by="msaid"))

ca_all
count(ca_all, county, msaid, msaname)

ca_all %>%
  filter(fyear>=2000, !vname %in% c("ptax_voter", "ptax_alloc")) %>%
  group_by(county, vname) %>%
  arrange(fyear) %>%
  mutate(pchya=value / lag(value) * 100 - 100) %>%
  # filter(county==link$cnty_ptax[2]) %>%
  ggplot(aes(fyear, pchya, colour=vname)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_x_continuous(name=NULL, breaks=seq(2000, 2020, 2)) +
  scale_y_continuous(name="% change", breaks=seq(-100, 100, 5), limits=c(NA, NA)) +
  ggtitle("Housing prices, assessed values, and property tax in selected California counties") +
  facet_wrap(~county, ncol=2) +
  theme_bw()

p <- ca_all %>%
  filter(fyear>=2000, !vname %in% c("ptax_voter", "ptax_alloc")) %>%
  mutate(vname=factor(vname, 
                      levels=c("hpi", "avtot", "ptax_tot"), 
                      labels=c("Housing prices", "Assessed value", "Property taxes"))) %>%
  group_by(county, vname) %>%
  arrange(fyear) %>%
  mutate(ivalue=value / value[fyear==2007] * 100) %>%
  # filter(county==link$cnty_ptax[2]) %>%
  ggplot(aes(fyear, ivalue, colour=vname)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 100, linetype="dashed") +
  geom_vline(xintercept = 2007, linetype="dotted") +
  scale_x_continuous(name=NULL, breaks=seq(2000, 2030, 2)) +
  scale_y_continuous(name="Index (2007=100)", breaks=seq(-100, 300, 20), limits=c(NA, NA)) +
  ggtitle("Housing prices, assessed values, and property tax in the 6 largest California counties") +
  facet_wrap(~county, ncol=2) +
  theme_bw() +
  theme(legend.title=element_blank())
p
ggsave(paste0("./data/", "CA_hpi_av_ptax.png"), p, scale=1.3, width=7, height=6, units="in")




#.. prop values hpi ----
m.hp2 <- readRDS("./data/msa_hpq.rds")
glimpse(m.hp2)
count(m.hp2, msaid, msaname) %>% filter(str_detect(msaname, ", CA"))
ca_hpi <- m.hp2 %>% filter(str_detect(msaname, ", CA"))


#.. prop tax ----
# https://propertytax.bythenumbers.sco.ca.gov/#!/year/default
# https://bythenumbers.sco.ca.gov/Raw-Data/Property-Tax-Raw-Data-for-Fiscal-Years-2003-2019/ifvn-u2fq

# PropertyTax_DataSet_20190513.xlsx
# LG_PROP_TAX_ALLOC_TOT
# total Total Allocation of Property Taxes 1% - Total_Countywide - Summary

fn <- "PropertyTax_DataSet_20190513.xlsx"
fnd <- "D:/Dropbox/Open Projects/LILP/PropertyTaxAndInstitutions/ProjectReport/CA/"
df <- read_excel(paste0(fnd, fn), sheet="LG_PROP_TAX_ALLOC_TOT")
glimpse(df)
# `Entity Name`
# `Fiscal Year...3`
# `Total Allocation of Property Taxes 1% - Total_Countywide - Summary` 

# the voter-approved taxes:
# Total Voter Approved Taxes Levied - Secured_Countywide - Summary
# Total Voter Approved Taxes Levied - Unsecured_Countywide - Summary
# Total Voter Approved Taxes Levied - HOPTR_Countywide - Summary
# Total Voter Approved Taxes Levied - Unitary and Operating Non-Unitary_Countywide - Summary

ca_ptax <- df %>%
  select(county=`Entity Name`, fyear=`Fiscal Year...3`, 
         ptax_alloc=`Total Allocation of Property Taxes 1% - Total_Countywide - Summary`,
         va_cntysec=`Total Voter Approved Taxes Levied - Secured_Countywide - Summary`,
         va_cntyunsec=`Total Voter Approved Taxes Levied - Unsecured_Countywide - Summary`,
         va_hoptr=`Total Voter Approved Taxes Levied - HOPTR_Countywide - Summary`,
         va_unitary=`Total Voter Approved Taxes Levied - Unitary and Operating Non-Unitary_Countywide - Summary`)

# %>% mutate(ptax_voter=)
glimpse(ca_ptax)
count(ca_ptax, county) # no total
count(ca_ptax, fyear)
summary(ca_ptax) # any NAs? NO
ca_ptax2 <- ca_ptax %>%
  mutate(ptax_voter=va_cntysec + va_cntyunsec + va_hoptr + va_unitary,
         ptax_tot=ptax_alloc + ptax_voter) %>%
  select(county, fyear, starts_with("ptax")) %>%
  gather(vname, value, -county, -fyear)

top5 <- ca_ptax2 %>%
  filter(fyear==2019) %>%
  top_n(5, value)

df2 %>%
  filter(county %in% top5$county) %>%
  group_by(county) %>%
  arrange(fyear) %>%
  mutate(pchya=ptax / lag(ptax) * 100 - 100) %>%
  ggplot(aes(fyear, pchya, colour=county)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0)



#.. assessed values ----

# https://www.boe.ca.gov/dataportal/dataset.htm?url=PropTaxAssessedValueCounty
fn <- "PropTaxAssessedValueCounty.csv"
fnd <- "D:/Dropbox/Open Projects/LILP/PropertyTaxAndInstitutions/ProjectReport/CA/"
df <- read_csv(paste0(fnd, fn))
glimpse(df)
count(df, County)

ns(df)

df %>%
  top_n(5, NetTotal)

df <- df %>%
  mutate(County=ifelse(County=="Totals", "Total", County)) %>% # 2013 yearfrom has bad name - but it also has bad record! same as 2014 yearfrom
  rename(pchya=YearToYearChange)

ca_avalues <- df %>% select(year1=AssessmentYearFrom, county=County, avtot=NetTotal)

df %>% filter(County=="Total") %>%
  arrange(AssessmentYearFrom) %>%
  mutate(pchya_calc=NetTotal / lag(NetTotal) * 100 - 100) %>%
  gather(vname, pch, pchya, pchya_calc) %>%
  ggplot(aes(AssessmentYearFrom, pch, colour=vname)) +
  geom_line() +
  geom_point()

df %>% filter(AssessmentYearFrom==2013) %>%
  top_n(5, NetTotal)

df %>% filter(County=="Total", AssessmentYearFrom==2017) %>%
  top_n(5, NetTotal)

# look at pchya of 5 biggest counties
top5 <- df %>%
  filter(County != "Total", AssessmentYearFrom==2018) %>%
  top_n(5, NetTotal)

df %>%
  rename(yfrom=AssessmentYearFrom) %>%
  filter(County %in% top5$County) %>%
  ggplot(aes(yfrom, pchya, colour=County)) +
  geom_line() +
  geom_point()
  




###################################################################################################################
# California
# BOE Annual report, especially Table 14; also, Table 5
# http://boe.ca.gov/annual/table5.htm -- the file named 2010 is for 2010-11, etc.
# http://boe.ca.gov/annual/annualrpts.htm
# http://boe.ca.gov/annual/statindex0910.htm
# http://boe.ca.gov/annual/annualrptsarchives.htm
# looks like we can (as of 1/8/2012) get the tables from here:
#     http://boe.ca.gov/annual/2010-11/appendix.html#stat

# first read in some statewide numbers
# net taxable assessed value - is the net after all exemptions from latest Table 4, div by 3
ntav<-ctov(scan( text="1,914,798.0 
      2,008,278.8 
      2,148,216.4 
      2,318,974.1 
      2,533,391.8 
      2,718,885.5 
      2,920,170.2 
      3,155,669.3 
      3,504,037.1 
      3,932,969.3 
      4,312,253.5 
      4,516,532.1 
      4,410,239.8 
      4,332,171.9
      4,344,340.2",what="character",sep="\n"))

# property tax levy from latest Table 4, div by 3
levy<-ctov(scan(text="20,459.3 
 21,415.9 
 22,890.3 
 24,767.1 
 27,163.3 
 29,351.3 
 31,812.1 
 34,520.8 
 38,340.9 
 43,155.8 
 47,211.2 
 49,840.5 
 49,184.3
 48,896.1
",what="character",sep="\n"))


catax<-data.frame(year=1998:2012,ntav=ntav,levy=c(levy,NA))
catax<-melt(catax,id="year")
catax$variable<-as.character(catax$variable)
cahpi<-subset(getdata("shpi"),stabbr=="CA" & qtr(date)==3 & year(date)>=1998)
cahpi$year<-year(cahpi$date)
cahpi$date<-NULL
cahpi$stabbr<-NULL
cahpi$variable<-"hpi"
cahpi
str(catax); str(cahpi)
catax2<-rbind.fill(catax,cahpi)
idx<-match(paste(2006,catax2$variable),paste(catax2$year,catax2$variable))
catax2$ivalue<-(catax2$value/catax2$value[idx]*100)
p<-qplot(year,ivalue,data=catax2,colour=variable,geom=c("point","line"),ylab="Index (2006=100)",xlab="\nSources: hpi: FHFA all-transactions index, 3rd calendar quarter\nntav and levy: CA BOE Statistical Appendix Table 4, using year of FY end",main="California housing prices (hpi)\nnet taxable assessed value (ntav), and property tax levy (levy)")+geom_hline(y=100)
p+scale_y_continuous(breaks=seq(30,140,10))


# now get values and levies from the annual statistical report
# read table 14, which has values and levies by county -- what about state borrowing???
# first, read in the years that are already in spreadsheets
gett14<-function(yr){ # use year in which fiscal year ENDS
  fnbase<-"_statistical_appendix"
  if(yr<=2010) fnend<-".xls" else fnend<-".xlsx"
  fn<-paste(yr-1,"-",substr(yr,3,4),fnbase,fnend,sep="")
  if(yr<=2008) t14<-"A-18 art14" else t14<-"Table 14"
  df<-read.xls(paste(cadir,fn,sep=""),sheet=t14,colClass="character", header=FALSE)
  df<-df[,c(1,5,15)]
  names(df)<-c("county","ntav","levy")
  df$county<-trim(df$county)
  df$county<-ifelse(grepl("total",df$county,ignore.case=TRUE),"Totals",df$county)
  df<-melt(df,id="county")
  df$value<-ctov(df$value)
  df<-subset(df,!is.na(value) & is.na(ctov(county)))
  df$variable<-as.character(df$variable)
  df$year<-yr
  df$value<-ifelse(df$year<=2008,df$value*1000,df$value)  
  return(df)
}
df2007<-gett14(2007)
df2008<-gett14(2008)
df2009<-gett14(2009)
df2010<-gett14(2010)
df2011<-gett14(2011)

# head(df2007); tail(df2007)

# now read in Tab 14 from the files I converted from Word and pdfs
gett14b<-function(yr){
  fn<-"CA StatAppendixTables.xlsx"
  sn<-paste("Tab14 ",yr,sep="")
  df<-read.xls(paste(cadir,fn,sep=""),sheet=sn,colClass="character", header=FALSE)
  # cols we want are 1,2, and 7
  df<-df[,c(1,2,7)]
  names(df)<-c("county","ntav","levy")
  df$county<-trim(df$county)
  df$county<-ifelse(grepl("total",df$county,ignore.case=TRUE),"Totals",df$county)
  df<-melt(df,id="county")
  df$value<-ctov(df$value)
  df<-subset(df,!is.na(value) & is.na(ctov(county)))
  df$value<-df$value*1000
  df$variable<-as.character(df$variable)
  df$year<-yr
  return(df)
}

df2006<-gett14b(2006)
df2005<-gett14b(2005)
df2004<-gett14b(2004)
df2003<-gett14b(2003)
df2002<-gett14b(2002)
df2001<-gett14b(2001)
df2000<-gett14b(2000)
df1999<-gett14b(1999)
df1998<-gett14b(1998)



tab14<-rbind(df1998,df1999,df2000,df2001,df2002,df2003,df2004,df2005,df2006,df2007,df2008,df2009,df2010,df2011)

# estimate net taxable assessed value for 2011 - get % change from table 7 of 2010, apply to 2010 values
tmp2011<-subset(tab14,year==2011 & variable=="ntav")
tmp2011
pch2012<-read.xls(paste(cadir,"2010-11_statistical_appendix.xlsx",sep=""),sheet="Table 7",colClass="character", header=FALSE)
head(pch2012,10)
pch2012<-pch2012[,c(1,15)]
pch2012<-pch2012[-c(1:8),]
names(pch2012)<-c("county","pch2012")
pch2012$pch2012<-ctov(pch2012$pch2012)
pch2012<-subset(pch2012,!is.na(pch2012))
pch2012$county[nrow(pch2012)]<-"Totals"
pch2012$county<-trim(pch2012$county)
head(pch2012,10); tail(pch2012)
tmp2012<-merge(tmp2011,pch2012)
tmp2012$year<-2012
tmp2012$value<-tmp2012$value*(1+tmp2012$pch2012/100)
tmp2012$pch2012<-NULL
tab14<-rbind(tab14,tmp2012)
#save(tab14,file=paste(cadir,"tab14.Rdata",sep=""))
saveRDS(tab14,file=paste0(cadir,"tab14.rds"))
# rm(df1998,df1999,df2000,df2001,df2002,df2003,df2004,df2005,df2006,df2007,df2008,df2009,df2010)


############################################################################################################
#load(file=paste(cadir,"tab14.Rdata",sep=""))
tab14<-readRDS(file=paste0(cadir,"tab14.rds"))
count(tab14,c("year","variable"))
head(tab14); tail(tab14)
idx<-match(paste(tab14$county,tab14$variable,2006),paste(tab14$county,tab14$variable,tab14$year))
tab14$ivalue<-tab14$value/tab14$value[idx]*100
idx<-match(paste(tab14$county,tab14$variable,tab14$year-1),paste(tab14$county,tab14$variable,tab14$year))
tab14$pchya<-(tab14$value/tab14$value[idx]-1)*100
tw<-dcast(tab14,county+variable~year,value.var="pchya")
head(tw)
tw
tw<-tw[order(tw$variable,tw[,"2011"]),]
subset(tw,variable=="levy")
subset(tw,county=="Totals")

write.csv(subset(tw,variable=="ntav"),file=paste(ptdir,"cantav.csv",sep=""))


tab14<-tab14[order(tab14$county,tab14$year,tab14$variable),]
# tab14$year<-as.numeric(tab14$year)
qplot(year,ivalue,data=subset(tab14,grepl("joaquin",county,ignore.case=TRUE)),colour=variable,geom=c("point","line"))+geom_hline(y=100)
#+scale_x_continuous(breaks=seq(2000,2015,1))


# now bring in hpi, linked to major counties
# merced, riverside, san benito, san joaquin (stockton), contra costa, solano (Vallejo), sacramento
# Link major counties to their msas
# Los Angeles 31100   31084   06037
# Orange 31100   42044   06059
# San Diego 41740           06073
# Santa Clara 41940           06085
# Alameda 41860   36084   06001
# riverside 40140 06065
# San Bernardino 40140           06071
# San Francisco 41860   41884   06075
# contra costa 41860 36084 06013
# San Mateo 41860   41884   06081
# Sacramento 40900           06067 
# Ventura 37100           06111
# And selected counties of interest:
# merced 32900 06047
# san benito 41940 06069
# san joaquin (stockton) 44700 06077
# solano (Vallejo) 46700 06095


# head(hpi)
camsahpi<-subset(getdata("msahpi"),grepl("CA",areaname))
count(camsahpi,"areaname")
# compute the average hpi for the typical county fiscal year
camsahpi$year<-ifelse(qtr(camsahpi$date) %in% c(1,2),year(camsahpi$date),year(camsahpi$date)+1)
camsahpi<-ddply(camsahpi, .(areaname,fips,year), summarise, value=mean(value, na.rm = TRUE))
camsahpi$variable<-"hpi"
head(camsahpi)
str(camsahpi)

#load(file=paste(cadir,"tab14.Rdata",sep=""))
tab14<-readRDS(file=paste0(cadir,"tab14.rds"))
tab14$areaname<-tab14$county
caall<-rbind.fill(tab14,camsahpi)
idx<-match(paste(caall$areaname,caall$variable,2007),paste(caall$areaname,caall$variable,caall$year))
caall$ivalue<-caall$value/caall$value[idx]*100
head(caall); tail(caall)
count(caall,"county")
# here are major counties in size order
e1<-expression(grepl("angeles",areaname,ignore.case=TRUE) | fips==31084)
e1<-expression(grepl("orange",areaname,ignore.case=TRUE) | fips==42044)
e1<-expression(grepl("diego",areaname,ignore.case=TRUE) | fips==41740)
e1<-expression(grepl("Santa Clara",areaname,ignore.case=TRUE) | fips==41940)
e1<-expression(grepl("francisco",areaname,ignore.case=TRUE) | fips==41884)
e1<-expression(grepl("Alameda",areaname,ignore.case=TRUE) | fips==36084)
e1<-expression(grepl("riverside",areaname,ignore.case=TRUE) | fips==40140)
e1<-expression(grepl("bernardino",areaname,ignore.case=TRUE) | fips==40140)
e1<-expression(grepl("contra",areaname,ignore.case=TRUE) | fips==36084)
e1<-expression(grepl("San Mateo",areaname,ignore.case=TRUE) | fips==41884)
e1<-expression(grepl("Sacramento",areaname,ignore.case=TRUE) | fips==40900)
e1<-expression(grepl("Ventura",areaname,ignore.case=TRUE) | fips==37100)
e1<-expression(grepl("merced",areaname,ignore.case=TRUE) | fips==32900)
e1<-expression(grepl("benito",areaname,ignore.case=TRUE) | fips==41940)
e1<-expression(grepl("joaquin",areaname,ignore.case=TRUE) | fips==44700)
e1<-expression(grepl("solano",areaname,ignore.case=TRUE) | fips==46700)
e1<-expression(grepl("solano",areaname,ignore.case=TRUE) | fips==46700)


qplot(year,ivalue,data=subset(caall,eval(e1) & year>=1998),colour=variable,geom=c("point","line"))+geom_hline(y=100)

xlab<-"Fiscal year"
ylab<-"Index (FY 2007=100)"
main<-"Housing prices, taxable assessed value, and property tax levy\nSan Joaquin County, CA"
main<-"San Joaquin County, CA\nHousing prices, taxable assessed value, and property tax levy"
vars<-c("hpi","ntav","levy")
e1<-expression(grepl("joaquin",areaname,ignore.case=TRUE) | fips==44700)
pdata<-subset(caall,eval(e1) & year>=1998 & variable %in% vars)
pdata$varf<-factor(pdata$variable,levels=vars,
                   labels=c("Housing price index (MSA)","Net taxable assessed value","Property tax levy"),ordered=TRUE)
p<-ggplot(data=pdata, aes(x=year, y=ivalue, group=varf),ylim=rep(30,140,10)) # use ggplot to gain complete control
p<-p + geom_line(aes(colour=varf,linetype=varf), size=1.2) # join observations with lines 
p<-p + geom_point(aes(colour=varf,shape=varf), size=2) # add points
p<-p + geom_hline(yintercept=100, size=1.4) # add a thicker reference line
# now start putting text on the graph
p<-p+xlab(xlab)+ylab(ylab)
p<-p+labs(colour="",linetype="",shape="")
p<-p+labs(title=main, plot.title=element_text(size=14))
p<-p+scale_y_continuous(limits=c(30,145),breaks=seq(40,140,20))
p<-p+scale_x_continuous(breaks=seq(1998,2014,2))
p
q<-p+theme(legend.position=c(.289,0.75),legend.background=element_rect(fill='white'))
ggsave(file=paste(cadir,"sanjoaquin.png",sep=""),plot=q,width=6)


dir<-"C:\\Users\\DonAMD\\Documents\\Data\\"

p+opts(legend.position=c(.25,0.6),legend.background=theme_rect(fill='white')) # works better for export plot

p<-qplot(year,ivalue,data=subset(caall,eval(e1) & year>=1998 & variable %in% c("ntav","levy","hpi")),colour=variable,geom=c("point","line"),main=main,xlab=xlab,ylab=ylab)
p+geom_hline(y=100)



str(tab14)

# special handling for 2007 (2006-07)
pt2007<-read.xls(paste(ptdir,"CA\\","table5_06 FromWord.xlsx",sep=""), colClass="character", header=FALSE)
names(pt2007)<-c("county","land","improvements","persprop","exemptions","nettotal","pch")
pt2007<-pt2007[-c(1:3),]
pt2007$county<-trim(pt2007$county)
pt2007[2:7]<-colwise(ctov,2:7)(pt2007)
pt2007$year<-2007 # 2006-07 fiscal year
# df$tmp<-df$land+df$improvements+df$persprop-df$exemptions-df$nettotal #  good
pt2007<-melt(pt2007,id=c("county","year"))
pt2007

getpt<-function(year){
  y2<-substr(year,3,4)
  df<-read.xls(paste(ptdir,"CA\\","table5_",y2,".xls",sep=""), colClass="character", header=FALSE)
  df<-df[,c(1,5,7,9,11,13,15)]
  # delete blank columns
  names(df)<-c("county","land","improvements","persprop","exemptions","nettotal","pch")
  df$county<-trim(df$county)
  df$county<-as.character(df$county)
  df<-subset(df,is.na(as.numeric(df$county))) # drop rows where county name is fully numeric as not what we want
  df[,2:7]<-colwise(ctov,2:7)(df)
  df$year<-year+1 # convert from assessment year to fiscal year I think
  df<-subset(df,!is.na(nettotal))
  df$county[nrow(df)]<-"Totals"
  df<-melt(df,id=c("county","year"))
  return(df)
}

pt2008<-getpt(2007)
pt2009<-getpt(2008)
pt2010<-getpt(2009)
pt2011<-getpt(2010)

ptall<-rbind(pt2007,pt2008,pt2009,pt2010,pt2011)
ptall$value<-ifelse(ptall$year>=2010,ptall$value/1e3,ptall$value)
head(ptall)

ptwide<-dcast(ptall,county+variable~year)
head(ptwide)
ptwide$pch2y2009<-(ptwide$"2009"/ptwide$"2007"-1)*100
ptwide$pch2y2011<-(ptwide$"2011"/ptwide$"2009"-1)*100
tmp<-subset(ptwide,variable=="nettotal", select=-c(2,4,6))
tmp<-tmp[order(tmp$pch2y2011),]
tmp

p<-ggplot(tmp, aes(x=pch2y2009, y=pch2y2011, label=county)) + 
  geom_text(size=3, colour="blue") + 
  #geom_vline(xintercept=mean(taxhpi$pch2y2009), colour="red", size=.2) +
  #geom_hline(yintercept=mean(taxhpi$taxpch), colour="red", size=.2) +
  geom_vline(xintercept=mean(tmp$pch2y2009), colour="gray", size=.4) +
  geom_hline(yintercept=0, colour="black", size=.8) +  
  scale_x_continuous("% change 2007 to 2009") + scale_y_continuous("% change 2009 to 2011") +
  opts(title="Change in California assessed property value by county")
p


