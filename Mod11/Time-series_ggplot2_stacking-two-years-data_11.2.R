
#######################Basic time series plotting using ggplot2
## 1. Import and detect data
library(tidyverse)
sstoi.pa <- read.table("sstoi_pa.txt", header=T)
glimpse(sstoi.pa)
SST2years <- sstoi.pa %>% 
  filter(YR<52) %>% 
  select(1:3) %>% 
  mutate(DAY=1)

##2.Create date column as. Date()


SST2years$Date <- paste(SST2years$DAY,SST2years$MON,SST2years$YR,sep=".")
head(SST2years$Date,3)
class(SST2years$Date)

SST2years$Date <- as.Date(SST2years$Date,"%d.%m.%y")
head(SST2years$Date,3)
class(SST2years$Date)

SST2years$Date <- format(SST2years$Date,"19%y-%m-%d")
head(SST2years$Date,3)
class(SST2years$Date)

SST2years$Date <- as.Date(SST2years$Date)
head(SST2years$Date,3)
class(SST2years$Date)


#####
SST2years <- sstoi.pa %>% 
  filter(YR<52) %>% 
  select(1:3) %>% 
  mutate(DAY=1) %>% 
  mutate(Date=as.Date(format(as.Date(paste(DAY,MON,YR,sep="."),"%d.%m.%y"),"19%y-%m-%d")))


##3.Format date axis labels: scale_x_date()
p <- ggplot(SST2years, aes(x=Date, y=NINO1.2)) +
  geom_point() + 
  geom_line() 
p

# To format date axis labels, 
# you can use different combinations of days, weeks, months and years:
# Weekday name: use %a and %A for abbreviated and full weekday name, respectively
# Month name: use %b and %B for abbreviated and full month name, respectively
# %d: day of the month as decimal number
# %Y: Year with century.
# See more options in the documentation of the function ?strptime
# Format : month/year

p+scale_x_date(date_labels = "%b")
p+scale_x_date(date_labels = "%Y %b %d")
p+scale_x_date(date_labels = "%W")
p+scale_x_date(date_labels = "%m-%Y")
p+scale_x_date(date_breaks = "10 week", date_labels = "%W")
p+scale_x_date(date_minor_breaks = "2 day")
p+scale_x_date(limit=c(as.Date("1950-01-01"),as.Date("1952-12-11")))

ggplot(SST2years, aes(x=Date, y=NINO1.2,color=as.factor(YR))) +
  geom_point() + 
  geom_line() +
  xlab("Date")+
  scale_x_date(date_labels = "%Y %b %d")+
  theme(axis.text.x=element_text(angle=30, hjust=1)) +
  guides(color=guide_legend(title = "Year"))

SST2years1 <- SST2years %>% mutate(MDAY=as.Date(paste(DAY,MON,sep="."),"%d.%m"))

####Stack the two years data by two lines 
ggplot(SST2years1,aes(x=MDAY, y=NINO1.2,color=as.factor(YR))) +
  geom_point() + 
  geom_line() +
  xlab("Date") +
  scale_x_date(date_labels = "%b")+
  theme(axis.text.x=element_text(angle=30, hjust=1)) +
  guides(color=guide_legend(title = "Year"))


