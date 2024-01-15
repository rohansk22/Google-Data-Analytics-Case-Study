install.packages("tidyverse")
install.packages("dplyr")
install.packages("janitor")
install.packages("modeest")
library(readxl)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
library(janitor)
library(dplyr)
library(modeest)

#importing the data

df_Dec <- read.csv("C:\\Users\\Rohan S K\\OneDrive\\Desktop\\Certificate\\Trip Data\\Dec_22.csv")
df_Jan <- read.csv("C:\Users\Rohan S K\OneDrive\Desktop\Certificate\Trip Data\Jan_23.csv")
df_Feb <- read.csv("C:\Users\Rohan S K\OneDrive\Desktop\Certificate\Trip Data\Feb_23.csv")
df_March <- read.csv("C:\Users\Rohan S K\OneDrive\Desktop\Certificate\Trip Data\Mar_23.csv")
df_April <- read.csv("C:\Users\Rohan S K\OneDrive\Desktop\Certificate\Trip Data\Apr_23.csv")
df_May <- read.csv("C:\Users\Rohan S K\OneDrive\Desktop\Certificate\Trip Data\May_23.csv")
df_June <- read.csv("C:\Users\Rohan S K\OneDrive\Desktop\Certificate\Trip Data\Jun_23.csv")
df_July <- read.csv("C:\Users\Rohan S K\OneDrive\Desktop\Certificate\Trip Data\Jul_23.csv")

#Merging the 4 data sets: 

#str(dataset_name)
str(df_Dec)
str(df_Jan)
str(df_Feb)
str(df_March)
str(df_April)
str(df_May)
str(df_June)
str(df_July)

#All the datasets have the dame number of variables and datatypes. We can start merging the 8 datasets

merged_df <- bind_rows(df_Dec, df_Jan, df_Feb, df_March, df_April, df_May, df_June, df_July)

#Cleaning names, spaces and parantheses, etc

merged_df <- clean_names(merged_df)

#Removing empty rows and columns: 
remove_empty(merged_df, which = c())


#Creating ride_length column 
merged_df$ride_length <- difftime(merged_df$ended_at, merged_df$started_at, units = 'mins')

#Creating day of the week column
merged_df$day_of_week <- wday(merged_df$started_at, label = T, abbr = T)

#Creating a starting Hour column
merged_df$starting_hour <- format(as.POSIXct(merged_df$started_at), '%H')

#Creating a starting Month column
merged_df$month <- format(as.Date(merged_df$started_at), '%m')

head(merged_df)

# Analyze

#Dropping NA values

merged_df <- drop_na(merged_df)

#Summary stats of the trip dataset
summary(merged_df)

#Checking the average ride length
mean(merged_df$ride_length)

#The average ride length for casual members 
mean(merged_df$ride_length[merged_df$member_casual == "casual"])

#The average ride length for annual members 
mean(merged_df$ride_length[merged_df$member_casual == "member"])

#The average ride length for casual and annual members
aggregate(merged_df$ride_length ~ merged_df$member_casual, FUN = mean)

#The median ride length for casual and annual members
aggregate(merged_df$ride_length ~ merged_df$member_casual, FUN = median)

#The most common day for renting bikes for casual and annual members
aggregate(merged_df$day_of_week ~ merged_df$member_casual, FUN = mfv)


#Data Visualizations 
#1 No. of rides by member types

options(scipen = 999)
ggplot(data = merged_df) +
  aes(x = day_of_week, fill = member_casual) +
  geom_bar(position = 'dodge') +
  labs(x = 'Day of week', y = 'Number of rides', fill = 'Member type', title = 'Number of rides by member type')
ggsave("number_of_rides_by_member_type.png")

#2 No. of rides per month 

options(scipen = 999)
ggplot(data = merged_df) +
  aes(x = month, fill = member_casual) +
  geom_bar(position = 'dodge') +
  labs(x = 'Month', y = 'Number of rides', fill = 'Member type', title = 'Number of rides per month')
ggsave("number_of_rides_per_month.png")

#3 Hourly use of bikes 
options(scipen = 999)
ggplot(data = merged_df) +
  aes(x = starting_hour, fill = member_casual) +
  facet_wrap(~day_of_week) +
  geom_bar() +
  labs(x = 'Starting hour', y = 'Number of rides', fill = 'Member type', title = 'Hourly use of bikes throughout the week') +
  theme(axis.text = element_text(size = 5))
ggsave("Hourly_use_of_bikes_throughout_the_week.png", dpi = 1000)

#4 Average trip duration between casual and annual members
options(scipen = 999)
ggplot(data=merged_df) + aes(x = member_casual, y = ride_length, fill = member_type) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(
    title = "Average Ride Duration by Member Type",
    x = "Member Type",
    y = "Average Ride Duration (minutes)"
  ) +
  theme_minimal()

aggregate(merged_df$ride_length ~ merged_df$member_casual + merged_df$day_of_week, FUN = mean)


#count will let you count, group & sort the unique values from a dataframe
#filter just filters out data that meets (==) or does not meet(!=) the requirement

count(filter(merged_df, member_casual=='member'), start_station_name, sort = T)
count(filter(merged_df, member_casual=='member'), end_station_name, sort = T)
count(filter(merged_df, member_casual=='casual'), start_station_name, sort = T)
count(filter(merged_df, member_casual=='casual'), end_station_name, sort = T)

write.csv(merged_df, file = "C:\\Users\\Rohan S K\\OneDrive\\Desktop\\Certificate\\merged_data.csv", row.names = FALSE)
getwd()
