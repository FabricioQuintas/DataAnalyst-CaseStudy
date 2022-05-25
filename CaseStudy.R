# For the process, that its a huge amount of data on the last 12 months, i'll use R to clean and navegate throught it 
# First, lets install and load the required packages.

install.packages("dplyr")
install.packages("readr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("scales")
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)

# Then, merge all files.csv (12 total) that are currently on a folder named "CaseStudy1"

datatrip <- list.files(path='C:/Users/fabri/Documents/CaseStudy1', pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

# Check columns names

colnames(datatrip)

# Delete all duplicates by ride_id

datatrip %>% distinct(ride_id, .keep_all = TRUE)

# Calculate the time of each ride, delete negative values and create a new variable with each week day.

datatrip <- datatrip %>% 
  mutate(riding_time = as.numeric((ended_at - started_at)/60))
datatrip$day_of_week <- wday(datatrip$started_at, label=TRUE, abbr=FALSE)
subset(datatrip, riding_time<0)

# Perfect, at this point we already merge all files required, delete duplicates by ride_id and created an value for ride duration and day of week
# Now, lets see how many rides did each type of user, and the preference of bike of them

members_count <- datatrip %>% 
  count(member_casual, rideable_type)
members_count %>% 
  ggplot(aes(member_casual, n, fill = rideable_type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Quantity of members, with bike preference",
    subtitle = "Data from June 2021 to May 2022",
    fill = "Bike type",
    x = "User type",
    y = "Number of users" ) +
  geom_text(
    aes(label = n), 
    position = position_stack(vjust = .5)) +
  scale_y_continuous(labels = label_number(suffix= "M", scale = 1e-6)) +
  annotate("text", x = 1, y = -80000, label = "Total = 2.536.358") +
  annotate("text", x = 2, y = -80000, label = "Total = 3.221.193")

# Let see the average of rides for each group (members/casuals)

datatrip %>%
  group_by(member_casual) %>%
  summarise_at(vars(riding_time), list(name = mean))

# With this graph, we can check the quantity of rides per day, separated by group

day_week_result <- datatrip %>% 
  count(member_casual, day_of_week, rideable_type)

day_week_result %>% 
  ggplot(aes(day_of_week, n, fill = rideable_type)) +
  facet_wrap(~member_casual) +
  geom_bar(stat = "identity") +
  labs(
    title = "Quantity of rides per day",
    subtitle = "Data from June 2021 to May 2022",
    fill = "Bike type",
    x = "Week day",
    y = "Number of Rides" ) +
  scale_y_continuous(labels = comma)+
  theme(axis.text.x=element_text(angle = 90))

# Done this phase


  