# installing packages

install.packages("ggplot2")
library(ggplot2)

install.packages("ggthemes")
library(ggthemes)

install.packages("lubridate")
library(lubridate)

install.packages("dplyr")
library(dplyr)

install.packages("tidyr")
library(tidyr)

install.packages("DT")
library(DT)

install.packages("scales")
library(scales)

install.packages("tidyverse")
library(tidyverse)

# Creating vectors of colors for the plots
colors = c("#CC1011", "#663333", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")
colors


# Reading in the datasets
apr14 <- read_csv("Downloads/Uber-dataset/uber-raw-data-apr14.csv")
View(apr14)
may14 <- read_csv("Downloads/Uber-dataset/uber-raw-data-may14.csv")
View(may14)
jun14 <- read_csv("Downloads/Uber-dataset/uber-raw-data-jun14.csv")
View(jun14)
jul14 <- read_csv("Downloads/Uber-dataset/uber-raw-data-jul14.csv")
View(jul14)
aug14 <- read_csv("Downloads/Uber-dataset/uber-raw-data-aug14.csv")
View(aug14)
sep14 <- read_csv("Downloads/Uber-dataset/uber-raw-data-sep14.csv")
View(sep14)

# Combine the data from all the months
uber_data <- rbind(apr14, may14, jun14, jul14, aug14, sep14)
cat("The dimensions of the data are:", dim(uber_data))

colnames(uber_data) <- c("Date.Time", "Lat", "Lon", "Base")

# Print the first few rows of data
head(uber_data)

#The uber_data table contains the column Date/Time which is a factor, Latitude and 
#...Longitudes which are double and Base which is factor. I will format the 
#...datetime into a more readable format using the Date Time conversion function.
uber_data$Date.Time <- as.POSIXct(uber_data$Date.Time, format="%m/%d/%Y %H:%M:%S")
uber_data$Time <- format(as.POSIXct(uber_data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
uber_data$Date.Time <- ymd_hms(uber_data$Date.Time)

# Create individual columns for month day and year
uber_data$day <- factor(day(uber_data$Date.Time))
uber_data$month <- factor(month(uber_data$Date.Time, label=TRUE))
uber_data$year <- factor(year(uber_data$Date.Time))
uber_data$dayofweek <- factor(wday(uber_data$Date.Time, label=TRUE))

# Add Time variables as well 
uber_data$second = factor(second(hms(uber_data$Time)))
uber_data$minute = factor(minute(hms(uber_data$Time)))
uber_data$hour = factor(hour(hms(uber_data$Time)))

head(uber_data)


#Data Visualisation
#Plotting the trips by hours in a day

hourly_data <- uber_data %>% 
  group_by(hour) %>% 
  dplyr::summarize(Total = n())

datatable(hourly_data)

# Plotting the trips by hour
p1 <- ggplot(hourly_data, aes(hour, Total)) + 
  geom_bar(stat="identity", 
           fill="cadetblue", 
           color="white") + 
  ggtitle("Trips Every Hour", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)

p1 + theme_classic()

# Aggregate the data by month and hour
month_hour_data <- uber_data %>% 
  group_by(month, hour) %>% 
  dplyr::summarize(Total = n())

p2 <- ggplot(month_hour_data, aes(hour, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Hour and Month") + 
  scale_y_continuous(labels = comma)

p2 + theme_classic()

# Aggregate data by day of the month 
day_data <- uber_data %>%
  group_by(day) %>% 
  dplyr::summarize(Trips = n())
day_data

# Plot the data for the day
p3 <- ggplot(day_data, aes(day, Trips)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Trips by day of the month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma)

p3 + theme_classic()

# Collect data by day of the week and month
day_month_data <- uber_data %>%
  group_by(dayofweek, month) %>%
  dplyr::summarize(Trips = n())
day_month_data

# Plot the above data
p4 <- ggplot(day_month_data, aes(dayofweek, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trips by Day and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

p4 + theme_classic()

month_data <- uber_data %>%
  group_by(month) %>% 
  dplyr::summarize(Total = n())
month_data

p5 <- ggplot(month_data, aes(month, Total, fill = month)) + 
  geom_bar(stat = "Identity") + 
  ggtitle("Trips in a month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

p5 + theme_classic()

# Heatmap visualization of day, hour and month
# Heatmap by Hour and Day
day_hour_data <- uber_data %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())
datatable(day_hour_data)

# Plot a heatmap 
p6 <- ggplot(day_hour_data, aes(day, hour, fill = Total)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Hour and Day")

p6 + theme_classic()

# Plot Heatmap by day and month
# Organize data by month and day
month_day_data <- uber_data %>%
  group_by(month, day) %>%
  dplyr::summarize(Trips = n())
month_day_data

# Plot heatmap 
p7 <- ggplot(month_day_data, aes(day, month, fill = Trips)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Month and Day")

p7 + theme_classic()

# Plot a heatmap by day of the week and month
p8 <- ggplot(day_month_data, aes(dayofweek, month, fill = Trips)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Month and Day")

p8 + theme_classic()

# Creating a map visualization of rides in NYC
# Set Map Constants
min_lat <- 40 
max_lat <- 40.91
min_long <- -74.15
max_long <- -73.7004

ggplot(uber_data, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "orange") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APRIL-SEPTEMBER)")
