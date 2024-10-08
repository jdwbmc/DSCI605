#strftime(),base: https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html
#lubridate package: https://www.rdocumentation.org/packages/lubridate/versions/1.9.2

library(tidyverse)
setwd("~/GitHub/DSCI605/Mod10")
######Three-year crime data from 2017-09-01 to 2020-08-31
crime_bat <- readRDS("Crime3years.rds")
## Select two-years
bat_train <- head(crime_bat, 365*24*2)

two_years_data <- crime_bat %>%
  filter(Date >= "2018-01-01" & Date <= "2019-12-31")

#Basic time series based on scatter plot
two_years_data %>% ggplot(aes(x = Date, y = Battery)) +
  geom_point(col = "maroon") + 
  geom_line(col = "red") +
  labs(title = "Battery Crime from 2018-01-01 to 2019-12-31 in Chicago", 
       x = "Hourly", y = "Number of Crimes") 


##Create new variables
bat_train_new<-two_years_data %>% 
  mutate(hour = hour(two_years_data$Date),week= weekdays(two_years_data$Date),  
         month = month(two_years_data$Date),year=year(two_years_data$Date)) %>% 
  mutate(weeknum= strftime(Date, format = '%V')) 


#How to group the data into Monthly (every 30/31 days) data
bat_train_monthly <- bat_train_new %>% 
  group_by(month,year) %>%
  summarize(case_monthly=sum(Battery)) %>%
  mutate(yearmonth=sprintf("%04d%02d", year, month))


#####First try for graphing
bat_train_monthly%>% 
  ggplot(aes(x = yearmonth, y = case_monthly,group=1)) +
  geom_point(col = "maroon") + geom_line(col = "red") +
  labs(title = "Battery Crime from 2018-01-01 to 2019-12-31 in Chicago", 
       x = "Month", y = "Number of Crimes") 

#########The best result
bat_train_monthly_sorted <- bat_train_monthly %>%
  arrange(yearmonth)
bat_train_monthly_sorted %>%
  ggplot(aes(x = yearmonth, y = case_monthly,group=1)) +
  geom_point(col = "maroon") + geom_line(col = "red") +
  labs(title = "Battery Crime from 2018-01-01 to 2019-12-31 in Chicago",
       x = "Year-Month", y = "Number of Crimes")+
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m")


glimpse(bat_train_monthly)
