library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(knitr)
library(bookdown)
library(ISOweek)

#And set working directory
setwd("~/GitHub/DSCI605/Mod11")

######Read the Three-year crime data from 2017-09-01 to 2020-08-31 into R
crime_bat <- readRDS("Crime3years.rds")
## Select two-years of data
two_years_data <- crime_bat %>%  #Go from 3 years to 2 years of data
  filter(Date >= "2018-01-01 00:00:00" & Date <= "2019-12-31 23:00:00")

glimpse(two_years_data)

class(two_years_data$Date)

bat_train_new <- two_years_data %>% 
  mutate(month = month(Date), year = year(Date), day = day(Date),week = week(Date), hour = hour(Date))

glimpse(bat_train_new)



bat_train_new <- bat_train_new[bat_train_new$year <= "2019", ]
glimpse(tyd2)

tyd3 <- bat_train_new %>%
  mutate(
    # Combine year and week into ISOweek format and convert to the Monday of that week
    ISO_Week_String = paste0(year, "-W", sprintf("%02d", week)),
    myDate = ISOweek2date(paste0(ISO_Week_String, "-1"))  # "-1" for Monday of the ISO week
  ) %>%
  mutate(myDate = if_else(week == 1 & year > min(year), myDate + 1, myDate))

glimpse(tyd3)

tyd3 <- tyd2
tyd3$Date <- as.Date(tyd3$Date)

glimpse(tyd3)

is_na_name <- is.na(tyd3$day)
sum(is_na_name)

battery_day <- tyd3 %>%
  group_by(day, month, year) %>%
  summarize(battery_by_day = sum(Battery, na.rm = TRUE)) %>%
  arrange(year, month, day)  # Sorting by year, then month, then day


battery_month <- tyd3 %>%
  group_by(month, year) %>%
  summarize(battery_by_month = sum(Battery, na.rm = TRUE)) %>%
  arrange(year, month) %>%  # Sorting by year, then month, then day
  mutate(yearmonth=sprintf("%04d%02d", year, month)) %>%  #Format yearmonth as a character type
  mutate(yearmonth = as.Date(paste0(substr(yearmonth, 1, 4), "-", 
                                  substr(yearmonth, 5, 6), "-01")))
glimpse(battery_month)
glimpse(battery_month$yearmonth)

battery_week <- tyd3 %>%
  mutate(
    ISO_Week_String = paste0(year, "-W", sprintf("%02d", week)),
    myDate = ISOweek2date(paste0(ISO_Week_String, "-1"))
  ) %>%
  group_by(week, year) %>%
  summarize(battery_by_week = sum(Battery, na.rm = TRUE), .groups = 'drop') %>%
  ungroup() %>%  # Add this line to retain all columns
  # Create a unique Date for each year-week
  mutate(myDate = ISOweek2date(paste0(year, "-W", sprintf("%02d", week), "-1"))) %>%
  arrange(year, week)
glimpse(battery_week)

### Hourly crime

#Basic time series based on scatter plot - 2 years of hourly data
bat_train_new %>% ggplot(aes(x = Date, y = Battery)) +
  geom_point(col = "maroon") +  #Plot hourly points
  geom_line(col = "red") +   #Draw lines between points
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M:%S") +
  labs(title = "Battery Crime from 2018-01-01 to 2019-12-31 in Chicago", 
       x = "Hourly", y = "Number of Crimes")

### weekly crime

#Basic time series based on scatter plot - 2 years of weekly data
battery_week %>% ggplot(aes(x = myDate, y = battery_by_week)) +
  geom_point(col = "maroon") +  #Plot weekly points
  geom_line(col = "red") +   #Draw lines between points
  scale_x_date(date_breaks = "4 months", date_labels = "%Y-%V") +
  labs(title = "Battery Crime from 2018-01-01 to 2019-12-31 in Chicago", 
       x = "Year-Week", y = "Number of Crimes")

#Now plot the improved data
battery_month %>%
  ggplot(aes(x = yearmonth, y = battery_by_month,group=1)) +
  geom_point(col = "maroon") + geom_line(col = "red") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") + #Change the x-scale so the x-axis is easier to read
  labs(title = "Battery Crime from 2018-01-01 to 2019-12-31 in Chicago",
       x = "Year-Month", y = "Number of Crimes")

battery_month2 <- battery_month %>% 
  mutate(
    MDAY = as.Date(paste(day(yearmonth), month(yearmonth), sep="."), "%d.%m")
  )

####Stack the two years data by two lines 
ggplot(battery_month2,aes(x=MDAY, y=battery_by_month,color=as.factor(year))) +
  geom_point() + 
  geom_line() +
  xlab("Date") +
  scale_x_date(date_labels = "%b")+
  theme(axis.text.x=element_text(angle=30, hjust=1)) +
  guides(color=guide_legend(title = "Year"))  
