library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readr)
library(hydroTSM)

August_2021_tripdata <- read_csv("Aug_2021.csv")
September_2021_tripdata <- read_csv("Sept_2021.csv")
October_2021_tripdata <- read_csv("Oct_2021.csv")
November_2021_tripdata <- read_csv("Nov_2021.csv")
December_2021_tripdata <- read_csv("Dec_2021.csv")
January_2022_tripdata <- read_csv("Jan_2022.csv")
February_2022_tripdata <- read_csv("Feb_2022.csv")
March_2022_tripdata <- read_csv("Mar_2022.csv")
April_2022_tripdata <- read_csv("Apr_2022.csv")
May_2022_tripdata <- read_csv("May_2022.csv")
June_2022_tripdata <- read_csv("June_2022.csv")
July_2022_tripdata <- read_csv("July_2022.csv")

bike_trips<-rbind(August_2021_tripdata,September_2021_tripdata,October_2021_tripdata,November_2021_tripdata,December_2021_tripdata,January_2022_tripdata,February_2022_tripdata,March_2022_tripdata,April_2022_tripdata,May_2022_tripdata,June_2022_tripdata,July_2022_tripdata)





bike_trips <- distinct(bike_trips)
bike_trips <- bike_trips %>% drop_na()
bike_trips%>%head(2)


bike_trips$ride_period <- as.integer(as.double(bike_trips$ride_period))
is.numeric(bike_trips$ride_period)

bike_trips$ride_period <- difftime(bike_trips$ended_at, bike_trips$started_at, units = "mins")

bike_trips$ride_period %>% head(10)
summary(as.numeric(bike_trips$ride_period))
bike_trips$started_at<-as.POSIXct(bike_trips$started_at)

bike_trips$start_hour<-format(bike_trips$started_at, "%H")

bike_trips$day<- format(bike_trips$started_at, "%a")

bike_trips$month<- format(bike_trips$started_at, "%b")

bike_trips %>% head(5)

unique(bike_trips$member_casual)

unique(bike_trips$rideable_type)

bike_trips[!(bike_trips$ride_period < 6.37 *1.5 | bike_trips$ride_period> 20.60 *1.5), ]

summary(as.numeric(bike_trips$ride_period))

is.character(bike_trips$member_casual)
bike_trips$ride_period <- as.numeric(as.character(bike_trips$ride_period))
is.numeric(bike_trips$ride_period)
str(bike_trips$ride_period)

bike_trips$avg_daily_ride<-aggregate(bike_trips$ride_period ~ bike_trips$member_casual + bike_trips$day , FUN=mean)

average_monthly_ride<-aggregate(bike_trips$ride_period ~ bike_trips$member_casual +bike_trips$month, FUN = mean)

average_monthly_ride

average_hourly_ride<-aggregate(bike_trips$ride_period ~ bike_trips$member_casual + bike_trips$start_hour, FUN = mean)

average_hourly_ride

average_ride_type<-aggregate(bike_trips$ride_period ~ bike_trips$member_casual + bike_trips$rideable_type, FUN = mean)

average_ride_type

table(bike_trips$member_casual)



daily_ride_trip<-bike_trips %>% 
  group_by(member_casual, day) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(day)

daily_ride_trip


hourly_ride_trip<-bike_trips %>% 
  group_by(member_casual, start_hour) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(start_hour)

hourly_ride_trip

monthly_ride_trip<-bike_trips %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(month)

monthly_ride_trip

unique(bike_trips$rideable_type)

table(bike_trips$rideable_type)
#rides number of rideable type by member and casual:
ride_no_member<-bike_trips%>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop')

ride_no_member

member_trips <- bike_trips[bike_trips$member_casual == 'member',]

top5_member_start_stations<-member_trips %>%drop_na(start_station_name) %>%
  group_by(start_station_name, member_casual,start_lat, start_lng)%>%
  summarise(station_count = n(), .groups='drop')%>%
  arrange(desc(station_count)) %>%head(n=5)
top5_member_start_stations

top5_member_end_stations<-member_trips %>%drop_na(end_station_name) %>%
  group_by(end_station_name, member_casual,end_lat, end_lng)%>%
  summarise(station_count = n(), .groups='drop')%>%
  arrange(desc(station_count)) %>%head(n=5)
top5_member_end_stations

casual_trips <- bike_trips[bike_trips$member_casual == 'casual',]

top5_casual_start_stations<-casual_trips %>%drop_na(start_station_name) %>%
  group_by(start_station_name, member_casual,start_lat, start_lng)%>%
  summarise(station_count = n(), .groups='drop')%>%
  arrange(desc(station_count)) %>%head(n=5)

top5_casual_end_stations<-casual_trips %>%drop_na(end_station_name) %>%
  group_by(end_station_name, member_casual,end_lat, end_lng)%>%
  summarise(station_count = n(), .groups='drop')%>%
  arrange(desc(station_count)) %>%head(n=5)

top_start_stations<-rbind(top5_member_start_stations, top5_casual_start_stations)
top_end_stations <-rbind(top5_member_end_stations, top5_casual_end_stations)

year_member_casual_distribution<-ggplot(bike_trips, aes(member_casual, fill=member_casual)) +geom_bar(width=0.5) +
  
  labs(x="Casuals & Members", title="Casuals and Members distribution",
       subtitle="the whole year ",
       captions=" the number of member rides is higher than 
the casual rides during the whole year.")

year_member_casual_distribution



member_casual_hour<-bike_trips %>%ggplot(aes(start_hour, fill=member_casual))+geom_bar()

member_casual_hour+labs(x="start_hour",
                        title="Distribution by start_hour", 
                        caption = " the most busy hours for all riders are in the afternoon from 15h to 19h.") +
  coord_flip()

bike_trips$day<- ordered(bike_trips$day, levels = c("Mon",  "Tue", "Wed","Thu", "Fri", "Sat", "Sun"))

member_casual_day<-ggplot(bike_trips, aes(day, fill=member_casual))+
  geom_bar()+coord_flip()

member_casual_day+labs(x="Week days",
                       title="Distribution by week days",
                       captions =  "the daily rides numbers of casual riders are higher
than the members riders on the weekends")

bike_trips$month<- ordered(bike_trips$month,
                           levels = c("Jan",  "Feb", "Mar","Apr", "May", "Jun", "Jul","Aug",  "Sep", "Oct","Nov", "Dec"))


bike_trips %>%
  ggplot(aes(month, fill=member_casual)) +
  geom_bar() +
  labs(x="Month", title="Distribution by month", 
       caption="  in the festival period the  rides number of casual is higher 
    than the members riders") + coord_flip()







ride_duration_month<-bike_trips %>% group_by(member_casual, month,rideable_type ) %>% 
  summarise(average_ride_period=mean(ride_period) ,.groups = 'drop') %>% 
  arrange(rideable_type)

ride_duration_month%>% ggplot(aes(month,as.numeric(average_ride_period), 
                                  colour= member_casual))+geom_point()+
  facet_wrap('rideable_type') +
  theme(axis.text.x = element_text(angle = 90))+
  labs(title="Monthly average rides durations by bikes type",
       caption = " the highest average is by casual riding docked bikes ")


ride_duration_day<-bike_trips %>% 
  group_by(member_casual, day,rideable_type ) %>%
  summarise(average_ride_period=mean(ride_period),.groups = 'drop') %>%
  arrange(rideable_type)

ride_duration_day%>%
  ggplot(aes(day,as.numeric(average_ride_period), colour= member_casual))+
  geom_point()+facet_wrap('rideable_type') +
  theme(axis.text.x = element_text(angle = 90))+
  labs(title="Daily average rides durations by bikes type", 
       caption = " the highest average is by casual riding docked bikes ")


type<-ggplot(bike_trips, aes(rideable_type, fill=member_casual)) +
  geom_bar() 

type+ labs(x="rideable_type", 
           title="rideable_type distribution",subtitle="the whole year ",
           captions=" the classic bikes are the most preferred by members ,
docked bikes the less but preferred only by casual .")


rides_numb_day<-bike_trips %>% 
  group_by(member_casual, day, rideable_type) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>% arrange(day)

rides_numb_day %>%
  ggplot(data=rides_numb_day, mapping=aes( x=day, y=number_of_rides, colour=member_casual))+
  geom_point()+facet_wrap("rideable_type")+labs(x="day",
                                                title = "Daily riders  bike type prefrences  ", 
                                                caption="docked bikes preferred only by casual riders") +
  theme(axis.text.x = element_text(angle = 90))

library(leaflet)

pal=colorFactor(palette=c("green","blue"),domain=bike_trips$member_casual)

top_start_stations%>% leaflet() %>% addTiles() %>% 
  setView(-87.6599,41.8793, zoom=11) %>%
  addCircleMarkers(data=top_start_stations,lat=~start_lat, 
                   lng = ~start_lng, color=~pal(member_casual),label=~as.character(member_casual))%>% 
  addLegend(position = "bottomright", pal=pal, 
            values=~member_casual, title="top_start_stations", opacity=1)

top_end_stations%>% leaflet() %>% addTiles() %>%
  setView(-87.6599,41.8793, zoom=11) %>%
  addCircleMarkers(data=top_end_stations,lat=~end_lat, lng = ~end_lng,
                   color=~pal(member_casual),label=~as.character(member_casual)) %>% 
  addLegend(position = "bottomright", pal=pal, 
            values=~member_casual, title="top_end_stations", opacity=1)


