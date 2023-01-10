install.packages("tidyverse")
install.packages("lubridate")
install.packages("hms")
install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(hms)
library(data.table)
library(dplyr)
library(ggplot2)

getwd()
setwd("C:/Users/Dell/Documents/Cyclistic_trip_data_for_previous_12_months/Cyclistic_trip_data_csv")

#loading csv files from October 2021 to September 2022
oct_2021_df <- read_csv("C:/Users/Dell/Documents/Cyclistic_trip_data_for_previous_12_months/Cyclistic_trip_data_csv/202110-divvy-tripdata.csv")
nov_2021_df <- read.csv("C:/Users/Dell/Documents/Cyclistic_trip_data_for_previous_12_months/Cyclistic_trip_data_csv/202111-divvy-tripdata.csv")
dec_2021_df <- read.csv("C:/Users/Dell/Documents/Cyclistic_trip_data_for_previous_12_months/Cyclistic_trip_data_csv/202112-divvy-tripdata.csv")
jan_2022_df <- read.csv("C:/Users/Dell/Documents/Cyclistic_trip_data_for_previous_12_months/Cyclistic_trip_data_csv/202201-divvy-tripdata.csv")
feb_2022_df <- read.csv("C:/Users/Dell/Documents/Cyclistic_trip_data_for_previous_12_months/Cyclistic_trip_data_csv/202202-divvy-tripdata.csv")
mar_2022_df <- read.csv("C:/Users/Dell/Documents/Cyclistic_trip_data_for_previous_12_months/Cyclistic_trip_data_csv/202203-divvy-tripdata.csv")
apr_2022_df <- read.csv("C:/Users/Dell/Documents/Cyclistic_trip_data_for_previous_12_months/Cyclistic_trip_data_csv/202204-divvy-tripdata.csv")
may_2022_df <- read.csv("C:/Users/Dell/Documents/Cyclistic_trip_data_for_previous_12_months/Cyclistic_trip_data_csv/202205-divvy-tripdata.csv")
jun_2022_df <- read.csv("C:/Users/Dell/Documents/Cyclistic_trip_data_for_previous_12_months/Cyclistic_trip_data_csv/202206-divvy-tripdata.csv")
jul_2022_df <- read.csv("C:/Users/Dell/Documents/Cyclistic_trip_data_for_previous_12_months/Cyclistic_trip_data_csv/202207-divvy-tripdata.csv")
aug_2022_df <- read.csv("C:/Users/Dell/Documents/Cyclistic_trip_data_for_previous_12_months/Cyclistic_trip_data_csv/202208-divvy-tripdata.csv")
sep_2022_df <- read.csv("C:/Users/Dell/Documents/Cyclistic_trip_data_for_previous_12_months/Cyclistic_trip_data_csv/202209-divvy-publictripdata.csv")

#check if the files loaded have same column names
colnames(oct_2021_df)
colnames(nov_2021_df)
colnames(dec_2021_df)
colnames(jan_2022_df)
colnames(feb_2022_df)
colnames(mar_2022_df)
colnames(apr_2022_df)
colnames(may_2022_df)
colnames(jun_2022_df)
colnames(jul_2022_df)
colnames(aug_2022_df)
colnames(sep_2022_df)

View(oct_2021_df)#have a look how the data looks


#merge all data frames into one data frame to make a complete year
cyclistic_df <- rbind(oct_2021_df,nov_2021_df,dec_2021_df,jan_2022_df,feb_2022_df,mar_2022_df,apr_2022_df,may_2022_df,jun_2022_df,jul_2022_df
                         ,aug_2022_df,sep_2022_df) 

View(cyclistic_df)
colnames(cyclistic_df)

#rename columns with clearer naming conversions
cyclistic_df <- cyclistic_df %>% rename(
                   "trip_id" = "ride_id"
                   ,"bikeid" = "rideable_type" 
                   ,"start_time" = "started_at" 
                   ,"end_time" ="ended_at" 
                   ,"from_station_name"= "start_station_name" 
                   ,"from_station_id" = "start_station_id"
                   , "to_station_name" = "end_station_name" 
                   ,"to_station_id" = "end_station_id"
                   ,"usertype" = "member_casual")

colnames(cyclistic_df)

print(cyclistic_df)

str(cyclistic_df)

#remove individual month data frames to clear up space in the environment
remove(oct_2021_df,nov_2021_df,dec_2021_df,jan_2022_df,feb_2022_df,mar_2022_df,apr_2022_df,may_2022_df,jun_2022_df,jul_2022_df
       ,aug_2022_df,sep_2022_df)

#drop values with NA
cyclistic_df <- na.omit(cyclistic_df)

# Filter out started_at data that are less than ended_at data, avoiding negative value
cyclistic_df <- cyclistic_df %>% filter(cyclistic_df$start_time < cyclistic_df$end_time)

#create new data frame to contain new columns
cyclistic_date <- cyclistic_df

#calculate ride length by subtracting ended_at time from started_at time and converted it to minutes
cyclistic_date$ride_length <- difftime(cyclistic_df$end_time, cyclistic_df$start_time, units = "mins")


#create columns for: day of week, month, day, year, time, hour
cyclistic_date$date <- as.Date(cyclistic_date$start_time) #default format is yyyy-mm-dd, use start date
cyclistic_date$day_of_week <- wday(cyclistic_df$start_time) #calculate the day of the week 
cyclistic_date$day_of_week <- format(as.Date(cyclistic_date$date), "%A") #create column for day of week
cyclistic_date$month <- format(as.Date(cyclistic_date$date), "%m")#create column for month
cyclistic_date$day <- format(as.Date(cyclistic_date$date), "%d") #create column for day
cyclistic_date$year <- format(as.Date(cyclistic_date$date), "%Y") #create column for year
cyclistic_date$time <- format(as.Date(cyclistic_date$date), "%H:%M:%S") #format time as HH:MM:SS
cyclistic_date$time <- as_hms((cyclistic_df$start_time)) #create new column for time
cyclistic_date$hour <- hour(cyclistic_date$time) #create new column for hour


#create column for different seasons: Spring, Summer, Autumn, Winter
cyclistic_date <-cyclistic_date %>% mutate(season = 
                                             case_when(month == "03" ~ "Spring",
                                                       month == "04" ~ "Spring",
                                                       month == "05" ~ "Spring",
                                                       month == "06"  ~ "Summer",
                                                       month == "07"  ~ "Summer",
                                                       month == "08"  ~ "Summer",
                                                       month == "09" ~ "Autumn",
                                                       month == "10" ~ "Autumn",
                                                       month == "11" ~ "Autumn",
                                                       month == "12" ~ "Winter",
                                                       month == "01" ~ "Winter",
                                                       month == "02" ~ "Winter")
)

#create column for different time_of_day: Night, Morning, Afternoon, Evening
cyclistic_date <-cyclistic_date %>% mutate(time_of_day = 
                                             case_when(hour == "0" ~ "Night",
                                                       hour == "1" ~ "Night",
                                                       hour == "2" ~ "Night",
                                                       hour == "3" ~ "Night",
                                                       hour == "4" ~ "Night",
                                                       hour == "5" ~ "Night",
                                                       hour == "6" ~ "Morning",
                                                       hour == "7" ~ "Morning",
                                                       hour == "8" ~ "Morning",
                                                       hour == "9" ~ "Morning",
                                                       hour == "10" ~ "Morning",
                                                       hour == "11" ~ "Morning",
                                                       hour == "12" ~ "Afternoon",
                                                       hour == "13" ~ "Afternoon",
                                                       hour == "14" ~ "Afternoon",
                                                       hour == "15" ~ "Afternoon",
                                                       hour == "16" ~ "Afternoon",
                                                       hour == "17" ~ "Afternoon",
                                                       hour == "18" ~ "Evening",
                                                       hour == "19" ~ "Evening",
                                                       hour == "20" ~ "Evening",
                                                       hour == "21" ~ "Evening",
                                                       hour == "22" ~ "Evening",
                                                       hour == "23" ~ "Evening")
)



#create a column for the month using the full month name
cyclistic_date <-cyclistic_date %>% mutate(month = 
                                             case_when(month == "01" ~ "January",
                                                       month == "02" ~ "February",
                                                       month == "03" ~ "March",
                                                       month == "04" ~ "April",
                                                       month == "05" ~ "May",
                                                       month == "06" ~ "June",
                                                       month == "07" ~ "July",
                                                       month == "08" ~ "August",
                                                       month == "09" ~ "September",
                                                       month == "10" ~ "October",
                                                       month == "11" ~ "November",
                                                       month == "12" ~ "December"
                                             )
)

print(cyclistic_df)
#remove duplicate rows
cyclistic_date <- distinct(cyclistic_date)
cyclistic_date <- cyclistic_date[!(cyclistic_date$ride_length <=0),] #remove where ride_length is 0 or negative
#remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
cyclistic_date <- cyclistic_date %>%  
  select(-c(trip_id, from_station_id, to_station_id,start_lat,start_lng,end_lat,end_lng)) 

#view the final data
View(cyclistic_date)

remove(apr_2021_df)

#------------------------------------Analyzing------------------------------------

#------------total rides------------

#total number of rides
nrow(cyclistic_date)

#----------member type-------

cyclistic_date %>%
  group_by(usertype) %>% 
  count(usertype)

#----------bike type --------

#total rides by member type 
cyclistic_date %>%
  group_by(usertype, bikeid) %>% 
  count(bikeid)

#total rides 
cyclistic_date %>%
  group_by(bikeid) %>% 
  count(bikeid)

#-----------hours-----------

#total rides by member type 
cyclistic_date %>%
  group_by(usertype) %>% 
  count(hour) %>% 
  print(n = 48) #lets you view the entire tibble

#total rides
cyclistic_date %>%
  count(hour) %>% 
  print(n = 24) #lets you view the entire tibble

max(cyclistic_date$ride_length)#longest ride
min(cyclistic_date$ride_length)#shortest ride


#----------------------TIME OF DAY-----------------------

#--------morning-------------

#total rides by member type 
cyclistic_date %>%
  group_by(usertype) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#total rides
cyclistic_date %>%
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)


#--------afternoon----------

#total rides by member type 
cyclistic_date %>%
  group_by(usertype) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#total rides 
cyclistic_date %>%
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)


#----------evening------------

#total rides by member type
cyclistic_date %>%
  group_by(usertype) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#total rides
cyclistic_date %>%
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)


#----------night-----------

#number of rides by member type
cyclistic_date %>%
  group_by(usertype) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#number of rides 
cyclistic_date %>%
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#--------all times of day--------

#total rides by member type 
cyclistic_date %>%
  group_by(usertype) %>% 
  count(time_of_day)

#number of rides
cyclistic_date %>%
  group_by(time_of_day) %>% 
  count(time_of_day)


#----------------DAY OF THE WEEK------------------

#total rides by member type
cyclistic_date %>%
  group_by(usertype) %>% 
  count(day_of_week)

#total rides 
cyclistic_date %>%
  count(day_of_week)


#----------------DAY OF THE MONTH-----------------

#total rides by member type
cyclistic_date %>%
  group_by(usertype) %>% 
  count(day) %>% 
  print(n = 62) #lets you view the entire tibble

#total rides
cyclistic_date %>%
  count(day) %>% 
  print(n = 31) #lets you view the entire tibble


#---------------------MONTH-----------------------

#total rides by member type 
cyclistic_date %>%
  group_by(usertype) %>% 
  count(month) %>% 
  print(n = 24) #lets you view the entire tibble

#total rides
cyclistic_date %>%
  count(month) 


#--------------------SEASON-----------------------

#--------spring-------

#total rides by member type 
cyclistic_date %>%
  group_by(usertype) %>% 
  filter(season == "Spring") %>% 
  count(season)

#total rides
cyclistic_date %>%
  filter(season == "Spring") %>% 
  count(season)

#--------summer----------

#total rides by member type
cyclistic_date %>%
  group_by(usertype) %>% 
  filter(season == "Summer") %>% 
  count(season)

#total rides
cyclistic_date %>%
  filter(season == "Summer") %>% 
  count(season)

#---------Autumn---------

#total rides by member type
cyclistic_date %>%
  group_by(usertype) %>% 
  filter(season == "Autumn") %>% 
  count(season)

#total rides
cyclistic_date %>%
  filter(season == "Autumn") %>% 
  count(season)

#---------winter----------

#total rides by member type
cyclistic_date %>%
  group_by(usertype) %>% 
  filter(season == "Winter") %>% 
  count(season)

#total rides 
cyclistic_date %>%
  filter(season == "Winter") %>% 
  count(season)

#-----all seasons-------

#total rides by member type
cyclistic_date %>%
  group_by(season, usertype) %>% 
  count(season)

#total rides
cyclistic_date %>%
  group_by(season) %>% 
  count(season)


#------------------------------------AVERAGE RIDE LENGTH-----------------------------------

#average of ride_length
cyclistic_avgRide <- mean(cyclistic_date$ride_length)
print(cyclistic_avgRide)


#------------------MEMBER TYPE--------------------

#average ride_length
cyclistic_date %>% group_by(usertype) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


#----------------TYPE OF BIKE---------------------

#total rides by member type 
cyclistic_date %>% group_by(usertype, bikeid) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length
cyclistic_date %>% group_by(bikeid) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


#-----------------------HOUR-------------------------

#average ride_length by member type
cyclistic_date %>% group_by(hour, usertype) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=48) #lets you view entire tibble

#average ride_length
cyclistic_date %>% group_by(hour) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24) #lets you view entire tibble


#--------------------TIME OF DAY---------------------

#----------morning------------

#average ride length by member type
cyclistic_date %>% 
  group_by(usertype) %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


#---------afternoon---------- 

#average ride length by member type
cyclistic_date %>% 
  group_by(usertype) %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


#--------evening------------

#average ride length by member type
cyclistic_date %>% 
  group_by(usertype) %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


#----------night---------

#average ride length by member type 
cyclistic_date %>% 
  group_by(usertype) %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))



#-------all times of day-------

#average ride length by member type
cyclistic_date %>% 
  group_by(time_of_day, usertype) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  group_by(time_of_day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


#-------------------DAY OF THE WEEK-----------------

#average ride_length by member type
cyclistic_date %>% group_by(usertype, day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length 
cyclistic_date %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))



#-----------------DAY OF THE MONTH------------------

#average ride_length by member type
cyclistic_date %>% group_by(day, usertype) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=62)  #lets you view entire tibble

#average ride_length
cyclistic_date %>% group_by(day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=31)  #lets you view entire tibble


#---------------------MONTH--------------------------

#average ride_length by member type
cyclistic_date %>% group_by(month, usertype) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24)  #lets you view entire tibble

#average ride_length
cyclistic_date %>% group_by(month) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


#----------------------SEASON-------------------------

#-----spring------

#average ride length by member type
cyclistic_date %>% 
  group_by(usertype) %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))



#---------summer---------

#average ride length by member type for summer 
cyclistic_date %>% 
  group_by(usertype) %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length for summer 
cyclistic_date %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))



#---------Autumn----------

#average ride length by member type
cyclistic_date %>% 
  group_by(usertype) %>% 
  filter(season == "Autumn") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  filter(season == "Autumn") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))



#---------winter--------

#average ride length by member type
cyclistic_date %>% 
  group_by(usertype) %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
cyclistic_date %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))



#--------all seasons--------

#average ride length by member type
cyclistic_date %>% 
  group_by(season, usertype) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length 
cyclistic_date %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#Popular Start Stations for Casual riders
cyclistic_date %>%
  group_by(usertype, from_station_name) %>%
  summarise(number_of_ride = n()) %>%
  filter(from_station_name != "", "casual"== usertype) %>%
  arrange(-number_of_ride) %>%
  head(n=30) %>%
  select(-usertype)


#Popular Start Stations for Member riders
cyclistic_date %>%
  group_by(usertype, from_station_name) %>%
  summarise(number_of_ride = n()) %>%
  filter(from_station_name != "", "member" == usertype) %>%
  arrange(-number_of_ride) %>%
  head(n=30) %>%
  select(-usertype)



#--------------Visualization for better analysis---------------------------------

cyclistic_date %>%
  group_by(bikeid, usertype) %>%
  summarize(count_trips = n()) %>%  
  ggplot(aes(x= bikeid, y=count_trips, fill=usertype, color=usertype)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme_bw()+
  labs(title ="Bicycle Type Number of trips", x = "Bicycle Type", y = "Count of Trips")


### Arranges the weekdays in order Sunday to Saturday.
cyclistic_date$day_of_week <- ordered(cyclistic_date$day_of_week,
                                         levels=c("Sunday", "Monday", "Tuesday", "Wednesday",
                                                  "Thursday", "Friday", "Saturday"))

### Number bike rides per week BAR CHART
cyclistic_date %>%
  group_by(usertype,day_of_week) %>%
  summarize(count_trips = n()) %>%  
  ggplot(aes(x= day_of_week, y=count_trips, fill=usertype, color=usertype)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme_bw()+
  labs(title ="Number of bike rides per Week", x = "Day", y = "Count of Trips")



### Number bike rides per month BAR CHART
cyclistic_date %>%
  group_by(usertype,month,year) %>%
  summarize(count_trips = n()) %>%  
  ggplot(aes(x= reorder(month, year), y=count_trips, fill=usertype, color=usertype)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme_bw()+
  labs(title ="Number of bike rides per month", x = "Month", y = "Count of Trips")


### Number bike rides per season BAR CHART
cyclistic_date %>%
  group_by(usertype,season) %>%
  summarize(count_trips = n()) %>%  
  ggplot(aes(x= season, y=count_trips, fill=usertype, color=usertype)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme_bw()+
  labs(title ="Number of bike rides per month", x = "Day", y = "Count of Trips")


### Number bike rides by time of day BAR CHART
cyclistic_date %>%
  group_by(usertype,time_of_day) %>%
  summarize(count_trips = n()) %>%  
  ggplot(aes(x= time_of_day, y=count_trips, fill=usertype, color=usertype)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme_bw()+
  labs(title ="Number of bike rides by time of day", x = "Day time", y = "Count of Trips")


#Average duration per day
cyclistic_date %>% 
  mutate(cyclistic_date$day_of_week <- wday(cyclistic_df$start_time)) %>% 
  group_by(usertype, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = as.numeric(mean(ride_length))) %>% 
  arrange(usertype, day_of_week)  %>% 
ggplot(aes(x = day_of_week, y = average_duration, fill = usertype)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme_bw()+
  labs(title ="Average duration per day", x = "Day", y = "Average duration (mins)") 



#Average duration per month
cyclistic_date %>% 
  mutate(cyclistic_date$month <- format(as.Date(cyclistic_date$date))) %>% 
  group_by(usertype, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = as.numeric(mean(ride_length))) %>% 
  arrange(usertype, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = usertype)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme_bw()+
  labs(title ="Average duration per month", x = "month", y = "Average duration (mins)") 


#Average duration by time of day
cyclistic_date %>% 
  mutate(cyclistic_date$hour <- hour(cyclistic_date$time)) %>% 
  group_by(usertype, hour) %>% 
  summarise(number_of_rides = n()
            ,average_duration = as.numeric(mean(ride_length))) %>% 
  arrange(usertype, hour)  %>% 
  ggplot(aes(x = hour, y = average_duration, fill = usertype)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme_bw()+
  labs(title ="Average duration per month", x = "hours", y = "Average duration (mins)")



ggplot(cyclistic_date, aes(bikeid, fill=usertype)) + 
  geom_bar() +
  labs(x="Riderable type", title = "Distibution by type of bikes") 



#-------------------Download data for tableau/power_bi visualization-----------------------

cyclistic_date$ride_length <- round(cyclistic_date$ride_length, digits = 1)
cyclistic_tableau <- cyclistic_date

#clean the data
#remove columns not needed: start_station_name, end_station_name, time, started_at, ended_at
cyclistic_tableau <- cyclistic_tableau %>%  
  select(-c(from_station_name, to_station_name, time, start_time, end_time))

View(cyclistic_tableau)

str(cyclistic_tableau)

#download the new data as a .csv file
fwrite(cyclistic_tableau,"cyclistic_data1.csv")





















