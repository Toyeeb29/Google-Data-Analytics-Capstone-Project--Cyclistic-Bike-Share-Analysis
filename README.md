# Google-Data-Analytics-Capstone-Project--Cyclistic-Bike-Share-Analysis
This is a capstone project carried out by me to demonstrate my problem-solving skills in data analysis after completing the Google Data Analytics Professional Certification Course

-------
# Introduction:
I'm a junior data analyst recently hired by Cyclistic Company to work with the marketing analysis team.
Cyclistic is a bike-share company in Chicago that feature more than 5800 bicycles and 600 dacking stations. Lily Morino is my manager and the director of marketing and he is responsible for the development of campaigns and initiative to promote the bike share program. My team is responsible for collecting,analyzing and reporting data that help guide Cyclistic marketing strategy. The approval of recommended marketing program is the task of the cyclistic executive team. Cyclistic has a flexible plan for the use of bikes,that the bike can be unlocked from one station and returned to any other station in the system any time. For the pricing: single_ride pass, full_day passes and are referred as casual_riders, riders who purchase an annual memberships and are referred as cyclistic_members. cyclistic finance analysts have concluded that annual_memberships are much more profitable than casual_riders. The Director of marketing believes that the future of the company success depends on maximizing the number of annual memberships and that will be the key to future growth too and there's a very good chance to convert casual_riders to cyclistic_members. Consequently, Morino set a clear goal which is to design marketing strategies aimed at converting casual_riders to cyclistic_members. By now my team wants to understand how casual riders and annual members use cyclistic bikes differently, from these insights the marketing team will design a new marketing strategy to convert casual riders to annual members. 

------
# Ask phase:
 Morino and the team are interested in analyzing the historical bikers trip data to identify trends. This future marketing program is guided by three questions:

1- how do annual members and casual riders use cyclistic bikes differently?

2- why would casual riders buy cyclistic annual membership?

3- how can cyclistic use digital media to influence casual riders to become members?

Morino has assigned me the first question, and this task is an opportunity for me as a junior data analyst to show my skills in the domain by applying my knowledge in this field of data analysis by following the different phases : ask, prepare, process, analyze, share and act.
The goal of the team is clear that is to have a successful marketing campaign toward the casual riders, in that we need to understand how the two types of riders behave differently? now that the asked question is clear: how do annual members and casual riders use Cyclistic bikes differently? all my job is to focus on this question in the aim to enhance the mission of my team by responding to the stakeholders expectations, enabling them to take a data driven decisions.

-------
# Prepare phase
The instruction of my manager is to use the historical data of the last twelve months collected by the company and kept herelink. and he gave me a week to finish this task.

I prepared a new folder in my pc called "bike_trips" to download the needed data.They are twelve files in zip format. The files are unzipped and each file is an csv file for one month period starting from August 2021 to July 2022. this data was provided by the and it's relevent, complete, comprehensive, current and cited.

------
# Process phase
R studio was used as a suitable tool for this task, it can enables me to perform different tasks such as importing the data,wrangling it, analyzing it, visualising it, documenting and sharing results by markdown files.​
To start with, data was imported to R studio after selecting appropriate directory the unzipped and downloaded CSV files was located on the computer. After which, necessary packages was installed and loaded into R Studio.



`` 
library(tidyverse)
library(lubridate)
``


## Importing the data

``
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
``


## Checking the imported data for consistency and accuracy

``
str(August_2021_tripdata)
str(September_2021_tripdata)
str(October_2021_tripdata)
str(November_2021_tripdata)
str(December_2021_tripdata)
str(January_2022_tripdata)
str(February_2022_tripdata)
str(March_2022_tripdata)
str(April_2022_tripdata)
str(May_2022_tripdata)
str(June_2022_tripdata)
str(July_2022_tripdata)
``

## Check for consistency in column and names
``
colnames(August_2021_tripdata)
colnames(September_2021_tripdata)
colnames(October_2021_tripdata)
colnames(November_2021_tripdata)
colnames(December_2021_tripdata)
colnames(January_2022_tripdata)
colnames(February_2022_tripdata)
colnames(March_2022_tripdata)
colnames(April_2022_tripdata)
colnames(May_2022_tripdata)
colnames(June_2022_tripdata)
colnames(July_2022_tripdata)
``
### It clearly shows that column are consistent with no errors, now we can bind our data together for easy analysis.

``
bike_trips<-rbind(August_2021_tripdata,September_2021_tripdata,October_2021_tripdata,November_2021_tripdata,December_2021_tripdata,January_2022_tripdata,February_2022_tripdata,March_2022_tripdata,April_2022_tripdata,May_2022_tripdata,June_2022_tripdata,July_2022_tripdata)
``
### understanding the data
``
glimpse(bike_trips)
``
### It shows that we have 5,860,776 rows and 13 columns
### Checking for duplicate
``
bike_trip<- bike_trips[!duplicated( bike_trips), ]
``
### No duplicate found
### Next thing to do is add a new column for rides period in minutes:

``
bike_trips$ride_period <- difftime(bike_trips$ended_at, bike_trips$started_at, units = "mins")
``

### Check the new added column

``
bike_trips$ride_period %>% head(10)
``

### Add columns that list the date, month, day,hour of each ride:
Convert 'started_at' column from character to POSIXct to extract hour,day,month from date


``
bike_trips$started_at<-as.POSIXct(bike_trips$started_at)
``

### Add columns that list the date, month, day,hour of each ride:

``
bike_trips$start_hour<-format(bike_trips$started_at, "%H")

bike_trips$day<- format(bike_trips$started_at, "%a")

bike_trips$month<- format(bike_trips$started_at, "%b")
``

### Now that we have cleaned and process our data, we can go to analysis phase


# Analysis phase
### Rides period for all riders: 
After exploring the ride_period values, we conclude that it is essential to remove the outliers with the aim to have meaningful values in our analysis(outliers= <1st qu. 1.5 & >3rd qu 1.5) 


``
bike_trips[!(bike_trips$ride_period < 6.37 *1.5 | bike_trips$ride_period> 20.60 *1.5), ]
``

### Convert "ride_period" from Factor to numeric to further run calculations


``
bike_trips$ride_period <- as.numeric(as.character(bike_trips$ride_period))
is.numeric(bike_trips$ride_period)
``


### Rides period : members vs casuals riders:
average on days of the week

``
bike_trips$avg_daily_ride<-aggregate(bike_trips$ride_period ~ bike_trips$member_casual + bike_trips$day , FUN=mean)
``

Average monthly ride

``
average_monthly_ride<-aggregate(bike_trips$ride_period ~ bike_trips$member_casual +bike_trips$month, FUN = mean)
``

Average hourly ride

``
average_hourly_ride<-aggregate(bike_trips$ride_period ~ bike_trips$member_casual + bike_trips$start_hour, FUN = mean)
``



