#Let's clear our workspace first
rm(list=ls())

#Get our working directory
getwd()
setwd("C:/Users/ethan/OneDrive/Documents/MyRCoursework/GoogleR") #Set your own working directory here.
#Here, I saved the data in the folder "Case_Study_Data" in the working directory.

#Now, let's load our packages.
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)

#Let's import our data. We'll use the first month as an example, and then run all other data through the same
#cleaning and filtering process.

#Remember that our aim here is to determine data across both time (duration/day of the week) and 
#space (lat/long, and presumably station).

#But first, let's wrap our data-cleaning workflow in a function:

clean<-function(filename) {
  dfholding<-read.csv(file=paste0("./Case_Study_Data/", filename)) %>% 
    clean_names() %>% #Get rid of any weird characters etc
    drop_na() %>% #Here we remove rows which contain NAs
    distinct() #Here we remove duplicates
  
get_dupes(dfholding) #Check if we missed out any dupes

#Convert datetimes from chr to datetime obj
dfholding$started_at<-ymd_hms(dfholding$started_at) 
dfholding$ended_at<-ymd_hms(dfholding$ended_at)

#Convert start_station_id and end_station_id to character
dfholding$start_station_id<-as.character(dfholding$start_station_id)
dfholding$end_station_id<-as.character(dfholding$end_station_id)

#Convert member_casual into factor with 2 levels
dfholding$member_casual<-as.factor(dfholding$member_casual)

#Inspect dataframe
str(dfholding)

#Return dataframe
return(dfholding)
}

#Testing function
jul20df<-clean("202007-divvy-tripdata.csv")

#Inspect dataframe
skim_without_charts(jul20df)
head(jul20df)
summary(jul20df)

#So far, we've cleaned 1 month of data. Let's clean the other 11.

aug20df<-clean("202008-divvy-tripdata.csv")
skim_without_charts(aug20df)
head(aug20df)

sep20df<-clean("202009-divvy-tripdata.csv")
skim_without_charts(sep20df)
head(sep20df)

oct20df<-clean("202010-divvy-tripdata.csv")
skim_without_charts(oct20df)
head(oct20df)

nov20df<-clean("202011-divvy-tripdata.csv")
skim_without_charts(nov20df)
head(nov20df)

dec20df<-clean("202012-divvy-tripdata.csv")
skim_without_charts(dec20df)
head(dec20df)

jan21df<-clean("202101-divvy-tripdata.csv")
skim_without_charts(jan21df)
head(jan21df)

feb21df<-clean("202102-divvy-tripdata.csv")
skim_without_charts(feb21df)
head(feb21df)

mar21df<-clean("202103-divvy-tripdata.csv")
skim_without_charts(mar21df)
head(mar21df)

apr21df<-clean("202104-divvy-tripdata.csv")
skim_without_charts(apr21df)
head(apr21df)

may21df<-clean("202105-divvy-tripdata.csv")
skim_without_charts(may21df)
head(may21df)

jun21df<-clean("202106-divvy-tripdata.csv")
skim_without_charts(jun21df)
head(jun21df)

#Combine all 12 months into 1 big dataframe.
all_trips<-bind_rows(jul20df, aug20df, sep20df, oct20df, nov20df, dec20df, jan21df, feb21df, 
                     mar21df, apr21df, may21df, jun21df)

#We note a small but persistent number of empty station IDs, so let's get rid of them.
all_trips<-filter(all_trips, start_station_id != "" & end_station_id != "")

summary(all_trips)
skim_without_charts(all_trips)

#Now that we have our big dataframe, We should now mutate it, adding our 2 time variables. 
#Of course, I intend to wrap this in another function.

transformdf<-function(dataframename){
  dataframename<-mutate(dataframename, duration_min = interval(started_at, ended_at)/dminutes(1), 
                        day_of_week = wday(started_at, label=TRUE), #Here we look at intraweek cycles
                        week_of_year = week(started_at), #Here we enforce weekly resolution
                        month_of_year = month(started_at, label=TRUE)) #Here we enforce monthly resolution
  dataframename<-filter(dataframename, duration_min>0) #We remove negative durations here.
  dataframename$rideable_type<-as.factor(dataframename$rideable_type) #We note there are three types of bikes.
  dataframename<-mutate(dataframename, season = case_when( #Here we create a new column dependent on previous input
    month_of_year == "Dec" ~ "Winter 2020",
    month_of_year == "Jan" ~ "Winter 2020",
    month_of_year == "Feb" ~ "Winter 2020",
    month_of_year == "Mar" ~ "Spring 2021",
    month_of_year == "Apr" ~ "Spring 2021",
    month_of_year == "May" ~ "Spring 2021",
    month_of_year == "Jun" ~ "Summer 2021",
    month_of_year == "Jul" ~ "Summer 2021",
    month_of_year == "Aug" ~ "Summer 2021",
    month_of_year == "Sep" ~ "Autumn 2020",
    month_of_year == "Oct" ~ "Autumn 2020",
    month_of_year == "Nov" ~ "Autumn 2020"))
  dataframename$season<-factor(dataframename$season, #Convert this to factor with 4 levels ordered correctly
                               levels = c("Autumn 2020", "Winter 2020", "Spring 2021", "Summer 2021")) 
  return(dataframename)
}

#Let's put this into practice.
all_trips_mutated<-transformdf(all_trips)

#Inspect combined dataset
summary(all_trips_mutated)
skim_without_charts(all_trips_mutated)

#Having cleaned and transformed our data, we can finally begin our analysis.
#As our business task states, we want to assess patterns which differ across members and casuals.

#Duration - average time spent on a single trip - Segmented seasonally.
#(we cannot identify riders,or people using the service regularly)

##Here we do this by season and visualise - This is our preferred option
aggregate(all_trips_mutated$duration_min, by = list(all_trips_mutated$season, all_trips_mutated$day_of_week, 
                                                    all_trips_mutated$member_casual), FUN=median) %>% 
  ggplot(aes(x=Group.2, y=x, group = Group.3, color = Group.3, linetype = Group.3)) + 
  #Group = Group.2 tells the program to link the lines by this category
  geom_line() + geom_point() +
  labs(title = "Casual riders take longer rides than members", subtitle = "Seasonal median daily trip duration", 
       caption = "Data from Divvy", y="Median trip duration (min)", x="Day of the week", color = "Rider type", 
       linetype = "Rider type") +
  scale_color_discrete(labels = c("Casual", "Member")) +
  scale_linetype_discrete(labels = c("Casual", "Member")) +
  facet_wrap(~Group.1)

###### NOT NEEDED ######

##Here we have our gross median, without weekly resolution - proxy for a length of a typical trip
setNames(aggregate(all_trips_mutated$duration_min, by = list(all_trips_mutated$member_casual), FUN=median), 
         c("rider_type", "median_trip_duration")) 

##And then by week, visualising as well - Too high res, not needed
aggregate(all_trips_mutated$duration_min, by = list(all_trips_mutated$week_of_year, 
                                                    all_trips_mutated$member_casual), FUN=median) %>% 
  ggplot(aes(x=Group.1, y=x, group = Group.2, color = Group.2, linetype = Group.2)) + 
  geom_line() + geom_point() +
  labs(title = "Casual riders take longer rides than members", subtitle = "Weekly median trip duration", 
       caption = "Data from Divvy", y="Median trip duration (min)", x="Week", color = "Rider type", 
       linetype = "Rider type") +
  scale_x_continuous(breaks = seq(0,55, len = 12))

###### NOT NEEDED ######

#Day of Week (Total number of rides, then seasonal comparison - stacked bar chart)

##Here, we want to segment by season. Months would be too data-heavy.
all_trips_mutated %>% 
  dplyr::count(season, day_of_week, member_casual) %>% 
  ggplot(aes(x=day_of_week, y=n, fill=member_casual)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~season) + 
  labs(title = "Members ride more regularly than casual riders", 
       subtitle = "Seasonal daily trip frequency", 
       caption = "Data from Divvy", y="Number of trips", x="Day of the week", fill = "Rider type") +
  scale_fill_discrete(labels = c("Casual", "Member")) +
  scale_y_continuous(labels = scales::comma) #Gets rid of scientific notation

####### NOT NEEDED ######

##Here, our total comparison - note that on weekdays, member rides outweigh casual rides.
all_trips_mutated %>% 
  dplyr::count(day_of_week, member_casual) %>% 
  ggplot(aes(x=day_of_week, y=n, fill=member_casual)) +
  geom_bar(stat = 'identity')

####### NOT NEEDED ######

#Bike Type (Check if all seasons show same tendency - stacked bar chart will do)
all_trips_mutated %>% 
  dplyr::count(season, rideable_type, member_casual) %>% 
  mutate(member_casual = case_when(
    member_casual == "member" ~ "Member",
    member_casual == "casual" ~ "Casual"
  )) %>% 
  ggplot(aes(x=member_casual, y=n, fill = rideable_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~season) + 
  labs(title = "No clear difference in bike type preference between members and casual riders", 
       subtitle = "Seasonal trip frequency", 
       caption = "Data from Divvy", y="Number of trips", x="Rider type", fill = "Bicycle type") +
  scale_fill_discrete(labels = c("Classic Bike", "Docked Bike", "Electric Bike"))+
  scale_y_continuous(labels = scales::comma) #Gets rid of scientific notation

#Station ranks - Top 5 for Casuals and Members

##Start Stations
###Pull out data and order by trip frequency
stations_ranked<-all_trips_mutated %>% 
  dplyr::count(start_station_name, start_lng, start_lat, member_casual)
stations_ranked<-arrange(stations_ranked, desc(n))

###Then filter them by rider type and take top 5
stations_ranked_members<-filter(stations_ranked, member_casual == "member") 
stations_ranked_members[1:5,]
stations_ranked_casual<-filter(stations_ranked, member_casual == "casual") 
stations_ranked_casual[1:5,]

###Collate ranks, update column names and write to csv for Tableau processing
stations_ranked_collated<-rbind(stations_ranked_casual[1:5,], stations_ranked_members[1:5,])
colnames(stations_ranked_collated)[5]<-"number_of_trips"
write.csv(stations_ranked_collated, "./Case_Study_Data/start_ranked_stations.csv")

##End Stations
###Pull out data and order by trip frequency
stations_ranked<-all_trips_mutated %>% 
  dplyr::count(end_station_name, end_lng, end_lat, member_casual)
stations_ranked<-arrange(stations_ranked, desc(n))

###Then filter them by rider type and take top 5
stations_ranked_members<-filter(stations_ranked, member_casual == "member") 
stations_ranked_members[1:5,]
stations_ranked_casual<-filter(stations_ranked, member_casual == "casual") 
stations_ranked_casual[1:5,]

###Collate ranks, update column names and write to csv for Tableau processing
stations_ranked_collated<-rbind(stations_ranked_casual[1:5,], stations_ranked_members[1:5,])
colnames(stations_ranked_collated)[5]<-"number_of_trips"
write.csv(stations_ranked_collated, "./Case_Study_Data/end_ranked_stations.csv")

##Journeys
###Pull out data and order by trip frequency
stations_ranked<-all_trips_mutated %>% 
  dplyr::count(start_station_name, start_lng, start_lat, end_station_name, end_lng, end_lat, member_casual) %>% 
  filter(start_station_name != end_station_name)
stations_ranked<-arrange(stations_ranked, desc(n))

###Then filter them by rider type and take top 5
stations_ranked_members<-filter(stations_ranked, member_casual == "member") 
stations_ranked_members[1:5,]
stations_ranked_casual<-filter(stations_ranked, member_casual == "casual") 
stations_ranked_casual[1:5,]

###Collate ranks, update column names and write to csv for Tableau processing
stations_ranked_collated<-rbind(stations_ranked_casual[1:5,], stations_ranked_members[1:5,])
colnames(stations_ranked_collated)[5]<-"number_of_trips"
write.csv(stations_ranked_collated, "./Case_Study_Data/ranked_journeys.csv")