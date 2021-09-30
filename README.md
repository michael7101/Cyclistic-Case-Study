# Case_Study_1

# Introduction
​
This analysis is for a Capstone Project from the Google Data Analytics Certificate. The project is based on the case study "Sophisticated, Clear, and Polished: Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). We will be using the Divvy dataset for our analysis. This project aims to collect, prepare, process, and analyze our given data source. Then share our data insights and visualizations to answer the key business question the scenario: "In what ways do the members and the casual riders use Divvy bikes differently?"
​
# Scenario
​
In the given scenario, I am a data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company's future success depends on maximizing the number of annual memberships. My team aims to understand how casual riders and annual members use Cyclistic bikes differently. These insights will help the marketing team design a new marketing strategy to convert casual riders into annual members.
​
**Key Stakeholders:**
​
* Primary Stakeholders - the director of marketing
* Secondary Stakeholders - the marketing analytics team and the executive team
​
**Business Task**
​
Analyze the Cyclistic’s historical trip data from the past 12 months to identify trends that differ between annual members and casual riders.
​
# Preparing Data For Exploration:
​
**Data Sources:**
​
The datasets used for this analysis are made available by Motivate Internation Inc. under this [licence](https://www.divvybikes.com/data-license-agreement). 
​
[Data Source link](https://divvy-tripdata.s3.amazonaws.com/index.html)
​
The data source is public and can be used to explore the different customer types. 
​
**Notes:** 
​
* The data privacy issues prohibit us from using riders’ personally identifiable information meaning that we can not connect past purchases to credit card numbers to determine if casual riders live in the Cyclistic service area or have purchased multiple passes.
​
* Cyclistic is a fictional company; hence the datasets contain the name of a different company. For this analysis, the given datasets can be used for our business task.
​
* The period analyzed is 12 months, from August 01, 2020, to September 31, 2020. 
​
* The data being souced can be relied on as the datasets are comprehensive, recent and initially sourced by Motivate Internation In, and appropriately licenced and cited. 
​
**List of Datasets:**
​
* 202009-divvy-tripdata 
* 202010-divvy-tripdata
* 202011-divvy-tripdata
* 202012-divvy-tripdata
* 202101-divvy-tripdata
* 202102-divvy-tripdata
* 202103-divvy-tripdata
* 202104-divvy-tripdata
* 202105-divvy-tripdata
* 202106-divvy-tripdata
* 202107-divvy-tripdata
* 202108-divvy-tripdata


```{r}
install.packages("tidyverse")
install.packages("ggmap")
install.packages("sqldf")
install.packages("geosphere")
install.packages("gridBase")
install.packages("gridExtra")
library(ggmap)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(grid)
library(sqldf)
library(geosphere)
library(gridBase)
library(gridExtra)

```


```{r}
library(readr)
X202009_divvy_tripdata <- read_csv("~/Project_1/202009-divvy-tripdata.csv")
X202010_divvy_tripdata <- read_csv("~/Project_1/202010-divvy-tripdata.csv")
X202011_divvy_tripdata <- read_csv("~/Project_1/202011-divvy-tripdata.csv")
X202012_divvy_tripdata <- read_csv("~/Project_1/202012-divvy-tripdata.csv")
X202101_divvy_tripdata <- read_csv("~/Project_1/202101-divvy-tripdata.csv")
X202102_divvy_tripdata <- read_csv("~/Project_1/202102-divvy-tripdata.csv")
X202103_divvy_tripdata <- read_csv("~/Project_1/202103-divvy-tripdata.csv")
X202104_divvy_tripdata <- read_csv("~/Project_1/202104-divvy-tripdata.csv")
X202105_divvy_tripdata <- read_csv("~/Project_1/202105-divvy-tripdata.csv")
X202106_divvy_tripdata <- read_csv("~/Project_1/202106-divvy-tripdata.csv")
X202107_divvy_tripdata <- read_csv("~/Project_1/202107-divvy-tripdata.csv")
X202108_divvy_tripdata <- read_csv("~/Project_1/202108-divvy-tripdata.csv")
```

# Compare column names of the files


```{r}
colnames(X202009_divvy_tripdata)
colnames(X202010_divvy_tripdata)
colnames(X202011_divvy_tripdata)
colnames(X202012_divvy_tripdata)
colnames(X202101_divvy_tripdata)
colnames(X202102_divvy_tripdata)
colnames(X202103_divvy_tripdata)
colnames(X202104_divvy_tripdata)
colnames(X202105_divvy_tripdata)
colnames(X202106_divvy_tripdata)
colnames(X202107_divvy_tripdata)
colnames(X202108_divvy_tripdata)
```

# Inspect the data frames and look for inconsistencies

```{r}
str(X202009_divvy_tripdata)
str(X202010_divvy_tripdata)
str(X202011_divvy_tripdata)
str(X202012_divvy_tripdata)
str(X202101_divvy_tripdata)
str(X202102_divvy_tripdata)
str(X202103_divvy_tripdata)
str(X202104_divvy_tripdata)
str(X202105_divvy_tripdata)
str(X202106_divvy_tripdata)
str(X202107_divvy_tripdata)
str(X202108_divvy_tripdata)
```

# Note: Files X202009_divvy_tripdata, X202009_divvy_tripdata, and X202009_divvy_tripdata have numerical columns on start_station_id and end_station_id.  The rest of the files have characters for these columns, which means X202009_divvy_tripdata, X202009_divvy_tripdata, and X202009_divvy_tripdata need to be changed characters so that they can stack correctly.


```{r}
X202009_divvy_tripdata <- mutate(X202009_divvy_tripdata, start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id))
X202010_divvy_tripdata <- mutate(X202010_divvy_tripdata, start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id))
X202011_divvy_tripdata <- mutate(X202011_divvy_tripdata, start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id))
```

# Stack 12 individual data frames into one data frame

```{r}
all_trips <- bind_rows(X202009_divvy_tripdata, X202010_divvy_tripdata, X202011_divvy_tripdata, X202012_divvy_tripdata, X202101_divvy_tripdata, X202102_divvy_tripdata, X202103_divvy_tripdata, X202104_divvy_tripdata, X202105_divvy_tripdata, X202106_divvy_tripdata, X202107_divvy_tripdata, X202108_divvy_tripdata)
```

### 3. Prepare Data for Analysis on Rides and Riders

# Inspect the new table (all_trips)

# dimensions of the data frame

```{r}
dim(all_trips)
```

 
# see the first 6 rows

```{r}
head(all_trips) 
```


# see list of columns and data types

```{r}
str(all_trips) 
```

# statistical summary of the data (mainly for numeric data)


```{r}
summary(all_trips)
```

# How many observations fall under each rider type

```{r}
table(all_trips$member_casual)
```

# Add columns that list the date, month, day, and year of each ride


```{r}
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%B")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%a")
```

# Calculate the "ride_length" and add a new column


```{r}
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at, units = "mins")
```

# Inspect the structure of the columns again

```{r}
str(all_trips)
```


# Convert "ride_length" from Factor to numeric to further run calculations


```{r}
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
```

# Remove trips that the ride length is <= 0 or more than one day (24 * 60 = 1440 minutes) and make a copy for the cleaned data frame

)
```{r}
all_trips_v2 <- all_trips[!(all_trips$ride_length > 1440 | all_trips$ride_length <= 0),]
str(all_trips_v2)
```
### 4. Create New Data Frame for Analyzing Station Usage

#Then the ride distance traveled in km

```{r}
all_trips_v2$ride_distance <- distGeo(matrix(c(all_trips_v2$start_lng, all_trips_v2$start_lat), ncol = 2), matrix(c(all_trips_v2$end_lng, all_trips_v2$end_lat), ncol = 2))
all_trips_v2$ride_distance <- all_trips_v2$ride_distance/1000
```

#At last the speed in Km/h


```{r}
all_trips_v2$ride_speed = c(all_trips_v2$ride_distance)/as.numeric(c(all_trips_v2$ride_length), units="hours")
```

#Fist we calculate the average distance, distance for both the casual and member type users:

```{r}
userType_means <- all_trips_v2 %>% group_by(member_casual) %>% summarise(mean_time = mean(ride_length),mean_distance = mean(ride_distance))

membervstime <- ggplot(userType_means) + 
                geom_col(mapping=aes(x=member_casual,y=mean_time,fill=member_casual), show.legend = FALSE)+
                labs(title = "Mean travel time by User type",x="User Type",y="Mean time in sec")

membervsdistance <- ggplot(userType_means) + 
                    geom_col(mapping=aes(x=member_casual,y=mean_distance,fill=member_casual), show.legend = FALSE)+
                    labs(title = "Mean travel distance by User type",x="User Type",y="Mean distance In Km",caption = "Data by Motivate International Inc")

grid.arrange(membervstime, membervsdistance, ncol = 2)

```

#The we check  the number of rides diferences by weekday:

```{r}
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length),.groups = 'drop') %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of rides by User type during the week",x="Days of the week",y="Number of rides",caption = "Data by Motivate International Inc", fill="User type") +
  theme(legend.position="top")
```


# It seems that the casual users travel the same average distance than the member users, but they have much longer rides, that would indicate a more leisure oriented usage vs a more "public transport" or pragmatic use of the bikes by the annual members.

# This idea is reinforced by the fact that annual users have a very stable use of the service during the week, but the casual users are more of a weekend user.


#Create a new data frame with only the rows with info in the "bike type" column:

```{r}
with_bike_type <- all_trips_v2 %>% filter(rideable_type=="classic_bike" | rideable_type=="electric_bike")
```

#Then lets check the bike type usage by user type:

```{r}
with_bike_type %>%
    group_by(member_casual,rideable_type) %>%
    summarise(totals=n(), .groups="drop")  %>%

ggplot()+
    geom_col(aes(x=member_casual,y=totals,fill=rideable_type), position = "dodge") + 
    labs(title = "Bike type usage by user type",x="User type",y=NULL, fill="Bike type") +
    scale_fill_manual(values = c("classic_bike" = "#746F72","electric_bike" = "#FFB100")) +
    theme_minimal() +
    theme(legend.position="top")
```


#And their usage by both user types during a week:


```{r}
with_bike_type %>%
    mutate(weekday = wday(started_at, label = TRUE)) %>% 
    group_by(member_casual,rideable_type,weekday) %>%
    summarise(totals=n(), .groups="drop") %>%

ggplot(aes(x=weekday,y=totals, fill=rideable_type)) +
  geom_col(, position = "dodge") + 
  facet_wrap(~member_casual) +
  labs(title = "Bike type usage by user type during a week",x="User type",y=NULL,caption = "Data by Motivate International Inc") +
  scale_fill_manual(values = c("classic_bike" = "#746F72","electric_bike" = "#FFB100")) +
  theme_minimal() +
  theme(legend.position="none")
```

# Here we can see that the annual members use both types of bikes for their rides, but the casual users show a clear preference for the electric bikes, which makes sense given the long duration of their rides.

# On a weekly basis we can see that for the annual members there is a small difference of usage between the start of the week, where they prefer the classic bike and the end of the week, where they use more electric bikes.

# For the casual users we see in general the same pattern of usage from the previous weekly charts, preferring the electric vs the classic bikes and having a weekend usage of the service.


#Lets check now the coordinates data of the rides, to see if is there any interesting pattern:

#First we create a table only for the most popular routes (>250 times)

```{r}
coordinates_table <- all_trips_v2 %>% 
filter(start_lng != end_lng & start_lat != end_lat) %>%
group_by(start_lng, start_lat, end_lng, end_lat, member_casual, rideable_type) %>%
summarise(total = n(),.groups="drop") %>%
filter(total > 250)
```
#Then we create two sub tables for each user type

```{r}
casual <- coordinates_table %>% filter(member_casual == "casual")
member <- coordinates_table %>% filter(member_casual == "member")
```

#Lets store bounding box coordinates for ggmap:

```{r}
chi_bb <- c(
  left = -87.700424,
  bottom = 41.790769,
  right = -87.554855,
  top = 41.990119
)
```

#Here we store the stamen map of Chicago

```{r}
chicago_stamen <- get_stamenmap(
  bbox = chi_bb,
  zoom = 12,
  maptype = "toner")
```

#Then we plot the data on the map

```{r}
ggmap(chicago_stamen,darken = c(0.8, "white")) +
   geom_curve(casual, mapping = aes(x = start_lng, y = start_lat, xend = end_lng, yend = end_lat, alpha= total, color=rideable_type), size = 0.5, curvature = .2,arrow = arrow(length=unit(0.2,"cm"), ends="first", type = "closed")) +
    coord_cartesian() +
    labs(title = "Most popular routes by casual users",x=NULL,y=NULL, color="User type", caption = "Data by Motivate International Inc") +
    theme(legend.position="none")

ggmap(chicago_stamen,darken = c(0.8, "white")) +
    geom_curve(member, mapping = aes(x = start_lng, y = start_lat, xend = end_lng, yend = end_lat, alpha= total, color=rideable_type), size = 0.5, curvature = .2,arrow = arrow(length=unit(0.2,"cm"), ends="first", type = "closed")) +  
    coord_cartesian() +
    labs(title = "Most popular routes by annual members",x=NULL,y=NULL, caption = "Data by Motivate International Inc") +
    theme(legend.position="none")
```


# The coordinates data resulted to be very interesting, as we can clearly see the casual is usually located around the center of the town, with all their trips located around that area which makes sense given that they have a more relaxed leisure rides, on weekends probably also tourist or sightseeing related rides, that naturally focus more on the downtown area where most of the interest points are.

# This contrasts heavily with the longer range of the annual users that connect the downtown with the outskirts of the city, that would suggest they are mostly people that live outside the downtown and use the service to commute everyday to their works in the city.




