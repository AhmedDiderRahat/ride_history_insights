---
title: "Insights of Bike Ride Data"
author: "Rahat, A.D."
date: "2023-06-18"
output:
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### 1. Dataset: [Google Drive Link of The Data](https://drive.google.com/file/d/1lu9XjTNqO3ZD50sPJE7IMs8Iy8glDsdK/view?usp=sharing)




### 2. Preparations

```{r}
rm(list = ls(all.names = TRUE))
```




### 3. Library Integration

```{r library_import, message=FALSE, warning=FALSE, paged.print=FALSE}
library("tidyverse")
library("ggplot2")
library("scales")
library("knitr")
library("cowplot")
```




### 4. Data Set Loading

```{r attached_warning, message=FALSE, warning=FALSE, paged.print=FALSE}
df <- read.csv('../dataset/original_bike_data.csv')
attach(df)
```




### 5. Data Cleaning


#### 5.1. Number of Rows

```{r}
total_data_point <- nrow(df)
cat(paste0('Total Number of data point in the data set: ', total_data_point))
```

There were an estimated **5.67 M** rows in the data set.



#### 5.2. Columns of the Data Set

```{r}
colnames(df)
```



#### 5.3. Null Value Analysis

```{r}
null_counts <- sapply(df, function(x) sum(is.na(x) | x == ""))
cat(paste0("The summary of null and empty values: \n\n"))
null_counts
```


The data-set exhibits a considerable level of cleanliness; however, some null and empty values persist in the station information-related columns. Given the substantial data volume, the removal of rows containing null values is not expected to significantly impact the overall data-set size.


```{r}
df <- na.omit(df)
```


Following the removal of null values from the 'end_lat' and 'end_lng' columns, a secondary examination of the dataset will be conducted to identify any remaining empty entries. To ensure a comprehensive analysis, both null and empty values will be meticulously reviewed in two distinct phases.



#### 5.4. Re-checking Null Entries

```{r}
null_counts <- sapply(df, function(x) sum(is.na(x)))
cat(paste0("The summary of null values: \n\n"))
null_counts
```


From the table it is clear, no null entries found in the data set.



#### 5.5. Checking Empty Instance

```{r}
null_counts <- sapply(df, function(x) sum(x == ""))
cat(paste0("The summary of the empty values: \n\n"))
null_counts
```


The data-set still contains certain instances of empty values, particularly in the 'start_station' and 'end_station' ID and name fields. Fortunately, since we possess the corresponding latitudes and longitudes for these features, we can confidently proceed with the existing data.



#### 5.6. Analyze the Amount of Data Loss After Null Elimination

```{r}
new_row_counts <- nrow(df)

reduction_rate <- (total_data_point - new_row_counts) * 100 / (total_data_point)
cat(paste0('New data points: ', new_row_counts, 
           ' | Reduce percentage: ', round(reduction_rate, 4), '%'))
```

So, after removing the null values only **0.1 %** data have been lost.




### 6. Data Processing


#### 6.1. Process Trip-Durations for Each Ride

```{r message=FALSE, warning=FALSE}
attach(df)
end_time <- as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
start_time <- as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# calculate the difference in secs
trip_duration <- difftime(end_time, start_time, units = "secs")

# add trip duration as a column
df$trip_duration <- trip_duration
rm(trip_duration)
attach(df)
```


##### 6.1.1. Get Lower and Upper Value of Trip-Duration

```{r}
min_td <- min(trip_duration)
max_td <- max(trip_duration)

cat(paste0('Lower Value of Trip Duration is: ', min_td, "\nUpper Value of Trip Duration is: ", max_td))
```


##### 6.1.2. Investigate the Negitive Trip-Duration

```{r}
# Get the rows that have negative trip-duration
new_df <- subset(df, trip_duration < 0) %>% 
  select("ride_id", "started_at", "ended_at", "trip_duration")

cat(paste("Total Number of Rows with Negative trip-duration is: ", nrow(new_df), 
          "\nTotal Deleted Precentage is: ", round((nrow(new_df)/new_row_counts*100), 4), "%" ))
```


Now see the sample of the Negative trip-duration time:
```{r}
head(new_df)
rm(new_df)
```

##### Summary: 
1. Total number of Negative trip-duration is 100.
2. As we have a large volume of data, we could remove them.


##### 6.1.3. Remove Negative trip-duration

```{r message=FALSE, warning=FALSE}
df <- filter(df, trip_duration >= 0)
attach(df)
```



#### 6.2. Rename the Membership Type

```{r}
# mapped the casual user as one-time user and annual users as subscribers
df$member_casual <- factor(df$member_casual, levels = c("casual", "member"), labels = c("one-time users", "subscribers"))
```



#### 6.3. Processing Day-of-Week from Start-Time

```{r message=FALSE, warning=FALSE}
# Converting string to Data-Time
start_time <-  as.POSIXct(df$started_at, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# All days from start time
df$day_of_week  <- weekdays(start_time)
```



#### 6.4. Processing Weekend/Weekdays Status from Start-Time

```{r}
# weekend status is 1 for weekend and 0 for weekdays
df$weekend_status <- ifelse(df$day_of_week == "Saturday" | df$day_of_week == "Sunday", 1, 0)

# mapped 1/0 to weekend/weekdays
df$weekend_status <- factor(df$weekend_status, levels = c(1, 0), labels = c("weekend", "weekdays"))
```



#### 6.5. Processing Month Name from Start-Time

```{r}
# take all months from the start time
df$months <- format(start_time, "%B")
```



#### 6.6. Processing Part-of-Day from Start-Time

```{r}
df$part_of_day <- ifelse(hour(start_time) < 6, "Night [9pm-6am)",
                         ifelse(hour(start_time) < 12, "Morning [6am-12pm)",
                                ifelse(hour(start_time) < 17, "Afternoon [12pm-5pm)",
                                ifelse(hour(start_time) < 21, "Evening [5pm-9pm)", "Night [9pm-6am)"))))

# remove the variable start_time
rm(start_time)
```



#### 6.7. Processing Round-Trip Status from Start-&-End Station Name

```{r message=FALSE, warning=FALSE}
# Calculate the round trip/one-way trip
df$is_round_trip <- ifelse(start_station_name == end_station_name, 1, 0)

# set the string variable instead of 0, and 1
df$is_round_trip <- factor(df$is_round_trip, levels = c(1, 0), labels = c("round trip", "one-way trip"))

attach(df)
```




### 7. Exploratory Data Analysis (EDA)


#### 7.1. Calculate Unique Number of Ride

```{r}
unique_ride_id <- length(unique(df$ride_id))
cat(paste0('Total number of ride in the data set: ', unique_ride_id))
```

There were an estimated **5.66 M** rides conducted during the year 2022.



#### 7.2. Calculate the Time Range of the Data

```{r}
cat(paste0('First trip date: ', min(started_at), ' | Last trip date: ', max(started_at)))
```

All the ride conducted within 1st January 2022 to 31st December 2022



#### 7.3. Analyze Trip-Duration


##### 7.3.1. Calculate Total Amount of Ride-Time

```{r}
td_sec <- sum(as.double(trip_duration))

td_min <- td_sec %/% 60
td_hr <- td_min %/% 60

trip_time_sec <- td_sec %% 60
trip_time_min <- td_min %% 60

cat(paste0('Total ride time: ', td_hr, " Hour: ", trip_time_min, " Min: ", trip_time_sec, " Sec."))
```

Total ride duration is approximately **1,540,956** Hours or **1.5 M** Hours.


##### 7.3.2. Calculate Avg. Ride Time for Each Ride

```{r}
ad_sec <- mean(as.double(trip_duration))

ad_min <- ad_sec %/% 60
ad_hr <- ad_min %/% 60

av_trip_time_sec <- ad_sec %% 60
av_trip_time_min <- ad_min %% 60

msg <- 'Average Ride Time: '

if (ad_hr > 0){
  msg <- paste0(msg, ad_hr, " Hour: ", av_trip_time_min, " Min: ", round(av_trip_time_sec, 2), " Sec.")
}else{ 
  msg <- paste0(msg, av_trip_time_min, " Min: ", round(av_trip_time_sec, 2), " Sec.")
}

cat(paste0(msg))
```

Average ride duration is approximately **16** Minutes and **20** Sec.



#### 7.4. Analyze the Membership Type 

```{r}
# set the custom color
colors <- c("one-time users" = "#3488c7", "subscribers" = "#808080")

# calculate the membership type wise counts
count_data <- df %>% count(member_casual)

total_dp <- sum(count_data$n)

count_data$percentage <- round(((count_data$n / total_dp) * 100), 2)

count_data$print_col <-paste0(count_data$n, " [", count_data$percentage, "%]")

cus_colors <- c("#3488c7", "#ED7014")

plt <- ggplot(count_data, aes(x = member_casual, y = n, fill = member_casual)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cus_colors) +
  geom_text(aes(label = print_col), vjust = -0.2) +
  labs(x = "Membership-Type", y = "Total Number of Ride") +
  ggtitle("Number of Ride in Different Membership-Type")+
  scale_y_continuous(labels = NULL) +
  theme(legend.position = "none")

ggsave("../documentation/images/fig_1_7.4_Number of Ride in Different Membership-Type.png", 
       plot = plt, width = 5, height = 4.5)

print(plt)
```


##### Summary:
1.  There are two different types of user subscriber and one-time user.
2.  One-time users used the ride approximately 2.3M times and subscriber used it 3.3M times.


##### 7.4.1 Analyze the Count Different Between the Membership Type

```{r}
# calculate the difference
diff <- as.numeric(abs(count_data[count_data$member_casual == "one-time users", 2] 
                       - count_data[count_data$member_casual == "subscribers", 2]))

diff_mul <- count_data[count_data$member_casual == "subscribers", 2] / 
  count_data[count_data$member_casual == "one-time users", 2]

cat(paste0('Difference between two type of member is: ', diff, 
           '.\nAnd subscribers member used ', round(diff_mul, 3),
           ' times more the the one-time users.'))
```


###### Summary:
1. Subscriber used approximately 1M more ride than the one-time users.
2. Which is 1.44 times higher.



#### 7.5. Analyze the Trip Duration or Total Trip Time


##### 7.5.1. Analyze the Total Trip Duration of each Membership Type

```{r fig.width=8}
# calculate the sum of all rides
tab_tt_mt <- aggregate(trip_duration ~ member_casual, data = df, FUN=sum)

# set the units as secs
tab_tt_mt$trip_duration <- as.numeric(tab_tt_mt$trip_duration, units = "secs")

# calculate into hour
tab_tt_mt$time_hour <- round(tab_tt_mt$trip_duration / 3600, 2)

# get total time in hours
total_tt_hour <- sum(tab_tt_mt$time_hour)

# calculate the percentage of both types
tab_tt_mt$percentage <- round(((tab_tt_mt$time_hour / total_tt_hour) * 100), 2)

tab_tt_mt$print_col <-paste0(tab_tt_mt$time_hour, " Hours [", tab_tt_mt$percentage, "%]")

cus_colors <- c("#3488c7", "#ED7014")

plt <- ggplot(tab_tt_mt, aes(x = member_casual, y = time_hour, fill = member_casual)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cus_colors) +
  geom_text(aes(label = print_col), vjust = -0.3) +
  xlab("Membership Type") +
  ylab("Time (Hours)") +
  ggtitle("Membership Type and Their Ride Time in Hours") +
  scale_y_continuous(labels = NULL) +
  theme(legend.position = "none")

ggsave("../documentation/images/fig_2_7.5.1_Membership Type and Their Ride Time in Hours.png", 
       plot = plt, width = 6.5, height = 4)

print(plt)

```


###### Summary:
1. One-time user ride 849,000.5 hours which is 55.1% of total ride. 
2. While the subscribers ride a total of 691,956.2 hours which is 44.9% of total trip duration.


##### 7.5.2. Analyze the Trip Duration Difference Between of each Membership Type

```{r}
# calculate the difference
diff_mul <- tab_tt_mt$time_hour[1] / tab_tt_mt$time_hour[2]

cat(paste0('One-time users ride ', round(diff_mul, 2), ' times more the the subscriber'))
```


###### Summary:
1.  Although the subscriber has 1.44 times higher ride number than the one-time user, but the one-time user have 1.23 times higher trip time than the subscriber.
2. So, we investigate the average trip-duration of each type.


##### 7.5.3. Analyze the Average Trip Duation of each Membership Type

```{r}
# get the average and median ride time of each group
avg_tt_mt <- df %>% 
  group_by(member_casual) %>% 
  summarise(
    mean_duration = as.numeric(mean(trip_duration)) / 60,
    median_duration = as.numeric(median(trip_duration)) / 60)

print(avg_tt_mt)
```


###### Summary:
1. The average ride time for one-time users is 21.98 minutes, contrasting with 12.41 minutes for subscribers, aligning with our expectations.
2. Notably, there exists a time disparity of nearly 9.5 minutes between the average ride duration of these two membership types.
3. Upon closer inspection, the median trip duration emerge at 12.97 minutes for one-time users and 8.33 minutes for subscribers.
4. This discrepancy in median values, roughly 4.64 minutes, while substantial, is only half of the difference observed in the average times.
5. The discernible variance between the mean and median trip duration for one-time users, suggesting approximately 9 minutes, points toward the potential existence of outliers within this group.


##### 7.5.4. Visualize the Average Trip Duation of each Membership Type

```{r}
plt <- ggplot(df, aes(x = member_casual, y = as.numeric(trip_duration) / 3600)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Trip Time in Different Membership type", x = "Membership Category", y = "Trip Time (Hours)")

ggsave("../documentation/images/fig_3_7.5.4_rip Time in Different Membership type.png", 
       plot = plt, width = 8, height = 6)

print(plt)

```


###### Summary:
1. Subscribers exhibit a more consistent pattern in their bike usage.
2. In the case of one-time users, who are frequently sporadic in their utilization, there are instances where extended ride duration are observed.



#### 7.6. Examine the Riding Patterns Across the Days of the Week


##### 7.6.1. Calculate the day of weeks and their Ride Counts

```{r}
## the custom order of day
week_day_order <- c("Monday",  "Tuesday", "Wednesday","Thursday", "Friday", "Saturday",  "Sunday")

# Count of the day of week and membership type
dw_df <- df %>% count(day_of_week)

dw_df$day_of_week <- factor(dw_df$day_of_week, levels = week_day_order, ordered = TRUE)

# Order the data
dw_df <- dw_df[order(dw_df$day_of_week), ]

dw_df
```


##### 7.6.2. Plot the Week Days and their Counts

```{r}
day_colors <- c(rep("#3488c7", 5), rep("#ED7014", 2))

plt <- ggplot(dw_df, aes(x=day_of_week, y = n, fill=day_of_week)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = day_colors) +
  geom_text(aes(label = n), vjust = -0.3) +
  xlab("Day of the Week") +
  ylab("Number of Ride") +
  ggtitle("Number of Ride in Different day of the week") +
  scale_y_continuous(labels = NULL) +
  theme(legend.position = "none")

ggsave("../documentation/images/fig_4_7.6.2_Number of Ride in Different day of the week.png", 
       plot = plt, width = 6, height = 4)

print(plt)
```


```{r}
mean_wd <-  mean(dw_df$n[dw_df$day_of_week != "Saturday" & dw_df$day_of_week != "Sunday"])
mean_we <-  mean(dw_df$n[dw_df$day_of_week == "Saturday" | dw_df$day_of_week == "Sunday"])

cat(paste0('Average number of ride on weekday: ', mean_wd, '\nAverage number of ride on weekend: ', mean_we))
```


###### Summary:
1. Ride distribution is well-balanced throughout the days of the week.
2. Notably, Saturday experiences the zenith of ride activity, while Monday records the most modest ride count.
3. However, the average ride count distinctly favors weekends over weekdays.


##### 7.6.3. Evaluate Membership Type Based on the Day of the Week

```{r}
# calculate the counts of ride based on membership type
tab_dw_mc <- (df %>% count(day_of_week, member_casual))

# order the data
tab_dw_mc$day_of_week <- factor(tab_dw_mc$day_of_week, levels = week_day_order)

tab_dw_mc <- tab_dw_mc[order(tab_dw_mc$day_of_week), ]

head(tab_dw_mc)
```


##### 7.6.4. Visualize Ride Counts for Different Days of the Week, Segmented by Membership Type

```{r fig.width=10}
# Create a grouped bar chart
plt <- ggplot(tab_dw_mc, aes(x = day_of_week, y = n, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Membership type") +
  ylab("Number of ride") +
  ggtitle("Number of Ride in Different day of weeks and Membership type") +
  labs(fill = "Membership Type") +
  scale_y_continuous(labels = NULL)

ggsave("../documentation/images/fig_5_7.6.4_Number of Ride in Different day of weeks and Membership type.png", 
       plot = plt, width = 6.5, height = 3.5)

print(plt)
```


###### Summary:
1. During weekdays, there is a consistent trend of higher ride usage among subscribers compared to one-time users.
2. Over weekends, the disparity diminishes, yet there remains a slight elevation in ride frequency among one-time users.


##### 7.6.5. Illustrate the Distribution of Ride Percentages for Different Membership Types, Categorized by Weekdays and Weekends

```{r fig.width=8}
# fetch the counts based on weekend status and membership type
tab_ws <- (df %>% count(weekend_status, member_casual))

# calculate the percentage
tab_ws <- tab_ws %>%
  group_by(member_casual) %>%
  mutate(Percentage = n / sum(n) * 100)

# plot the data
plt <- ggplot(tab_ws, aes(x = "", y = Percentage, fill = weekend_status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "weekend_status") +
  theme_void() +
  facet_wrap(~ member_casual, strip.position = "bottom") +
  labs(fill = "Weekend Status") +
  geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5)) + 
  ggtitle("Ride Distribution for Different Membership Types, Based on Weekdays and Weekends")

ggsave("../documentation/images/fig_6_7.6.5_Ride Distribution for Different Membership Types, 
       Based on Weekdays and Weekends.png", 
       plot = plt, width = 6, height = 2.5)

print(plt)
```


###### Summary:
1. The majority of subscribers (75%) opt for rides on weekdays, while a minority (25%) prefer weekends.
2. For one-time users, 37% of their rides occur on weekends.
3. This highlights that one-time users exhibit a stronger preference for weekend rides compared to the more weekday-centric pattern of subscribers.



#### 7.7. Analyze Riding Patterns Throughout the Months of the Year 


##### 7.7.1. Visualize the Monthewise Counts of Ride

```{r fig.width=10}
# get the monthly counts
tab_mc <- (df %>% count(months))

# months order
months_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")


# set the custom factor level
tab_mc$months <- factor(tab_mc$months, levels = months_order)

# sort the data frame
tab_mc <- tab_mc[order(tab_mc$months), ]

# mark the sunny session
month_colors <- c(rep("#3488c7", 4), rep("#ED7014", 6), rep("#3488c7", 2))

plt <- ggplot(tab_mc, aes(x=months, y = n, fill=months)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = month_colors) +
  geom_text(aes(label = n), vjust = -0.3) +
  xlab("Months of Year") +
  ylab("Number of ride") +
  ggtitle("Number of Ride in Different months of the year") +
  scale_y_continuous(labels = NULL) +
  theme(legend.position = "none")

ggsave("../documentation/images/fig_7_7.1.1_Ride Distribution for Different Membership Types, 
       Based on Weekdays and Weekends.png", 
       plot = plt, width = 7.5, height = 4.3)

print(plt)
```


##### 7.7.2. Calculate the percentage of Ride in Summer

```{r}
total_ride <- nrow(df)

summer_months <- c("May", "June", "July", "August", "September", "October")

ss_ride <- nrow(df[df$months %in% summer_months, ])

msg <- paste0("Total ", round((ss_ride / total_ride * 100), 2), " % of ride recorded at Summer") 

cat(paste0(msg))
```


###### Summary:
1. The distribution of the data is look-like normal distributed.
2. The highest numbers of ride captured in July which is 822,525.
3. The lowest counts recorded on January which is 103,684.
4. The number of ride is dramatically increase in the summer season (May-October).
5. 3 out of 4 rides recorded at summer while only 1 recorded at winter.


##### 7.7.3. Visualize the  Riding Pattern for Membership Type on Different Months of the Year

```{r fig.width=10}
# get the monthly counts
tab_mm_mc <- (df %>% count(months, member_casual))

tab_mm_mc$months <- toupper(substr(tab_mm_mc$months, 1, 3))

month_short_form <- toupper(substr(months_order, 1, 3))

# set the custom factor level
tab_mm_mc$months <- factor(tab_mm_mc$months, levels = month_short_form)

# sort the data frame
tab_mm_mc <- tab_mm_mc[order(tab_mm_mc$months), ]

plt <- ggplot(tab_mm_mc, aes(x=months, y = n, fill=member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Months") +
  ylab("Number of ride") +
  ggtitle("Ride Distribution in Different Months of the year for Different Membership Types") +
  labs(fill = "Membership type") +
  scale_y_continuous(labels = NULL)

ggsave("../documentation/images/fig_8_7.7.3_Ride Distribution in Different Months 
       of the year for Different Membership Types.png", 
       plot = plt, width = 7, height = 4.5)

print(plt)
```


###### Summary:
1.  Throughout the year, subscribers use more ride than the one-time users.
2.  The increasing trends for summer is clearly presented in the plot, but if we consider the membership type then the one-time users has dramatic upward trend in the summer season.


##### 7.7.4. Visualize the Riding Pattern for Membership Type on Different Seasone

```{r fig.width=10}
# set a variable for summer season is_summer = 1, then summer 0 otherwise
tab_mm_mc$is_summer <- ifelse(tab_mm_mc$months == "May" | tab_mm_mc$months == "June" | tab_mm_mc$months == "July" |  
                                tab_mm_mc$months == "August" | tab_mm_mc$months == "September" | 
                                tab_mm_mc$months == "October", 1, 0)

tab_ss <- tab_mm_mc[c("member_casual", "is_summer", "n")]

# calculate the summary of member type and is_summer
tab_ss <- tab_ss %>%
  group_by(member_casual, is_summer) %>%
  summarise(n = sum(n), .groups = "drop") %>% 
  group_by(member_casual) %>%
  mutate(Percentage = n / sum(n) * 100)


# set the string variable instead of 0, and 1
tab_ss$is_summer <- factor(tab_ss$is_summer, levels = c(1, 0), labels = c("summer(May-Oct.)", "winter"))


# make the plot
plt <- ggplot(tab_ss, aes(x = "", y = Percentage, fill = is_summer)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "is_summer") +
  facet_wrap(~ member_casual, strip.position = "bottom") +
  theme_void() +
  labs(fill = "Season") +
  ggtitle("Total Ride Percentage based Season for Different Type of Members") +
  geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5))

ggsave("../documentation/images/fig_9_7.7.4_Total Ride Percentage based Season 
       for Different Type of Members.png", 
       plot = plt, width = 6, height = 2.5)

print(plt)
```


###### Summary:
1. One-time user rides 83% in the summer time.
2. While the subscribers rides 70% on summer and 30% on the winter.
3. So, the one-time users exhibit a strong affinity for the summer season.


#### 7.8. Analyze the Riding Pattern in Diffrent Parts of Day 


##### 7.8.1. Visualize the Total Rides Counts in Diffrent Parts of the Day

```{r fig.width=10}
# calculate the total ride counts based on different part of day
tt_pfd <- df %>% count(part_of_day)

# custom factor 
custom_part_of_day <- c("Morning [6am-12pm)", "Afternoon [12pm-5pm)", "Evening [5pm-9pm)", "Night [9pm-6am)")

# set the custom factor level
tt_pfd$part_of_day <- factor(tt_pfd$part_of_day, levels = custom_part_of_day)

# sort the data frame
tt_pfd <- tt_pfd[order(tt_pfd$part_of_day), ]

# calculate the %
tt_pfd$percentage <- round((tt_pfd$n / sum(tt_pfd$n) * 100), 2)

tt_pfd$percentage <- paste(tt_pfd$percentage, "%", sep = "")

custom_colors <- c("#FF9999", "#66CC66", "#9999FF", "#FFCC66")

# plot the analysis
plt <- ggplot(tt_pfd, aes(x = part_of_day, y = n, fill = part_of_day)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percentage), vjust = -0.2) +
  labs(x = "Part of The Day", y = "Total Number of Ride") +
  ggtitle("Number of Ride in Different Part of The Day") +
  scale_y_continuous(labels = NULL) +
  theme(axis.text.x = element_blank())

# Customize the legend
legend_labels <-  paste(tt_pfd$part_of_day, "-", tt_pfd$n)  # Create legend labels
plt <- plt + scale_fill_manual(values = custom_colors, labels = legend_labels, 
                                 name = "Part of the Day with Count")


ggsave("../documentation/images/fig_10_7.8.1_Number of Ride in Different 
       Part of The Day.png", 
       plot = plt, width = 6, height = 4)

# Display the plot
print(plt)
```


###### Summary: 
1. Most busy part of the day is Afternoon time (between 12PM to 5PM), 33.57 % of rides occurred in the time frame.
2. Evening time is the second busiest time zone which contains 29.4% of total ride.
3. 23.79% of the total rides recorded in the morning time.
4. Night time is the least busy time zone in a day.


##### 7.8.2. Examine the Riding Patterns Across the Parts of the Day for Different Users

```{r fig.width=8}
# calculate the total ride counts based on different part of day
tab_tc <- (df %>% count(part_of_day, member_casual))

tab_tc <- tab_tc %>%
  group_by(member_casual) %>%
  mutate(Percentage = n / sum(n) * 100) %>% 
  arrange(member_casual)


## order the data
tab_tc$part_of_day <- factor(tab_tc$part_of_day, levels = custom_part_of_day)


plt <- ggplot(tab_tc, aes(x = "", y = Percentage, fill = part_of_day)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "part_of_day") +
  theme_void() +
  facet_wrap(~ member_casual, strip.position = "bottom") +
  labs(fill = "Part of the Day") +
  geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5)) + 
  ggtitle("Totat Ride Distribution in Different Part-of-the-Day for 2-type of Users")

ggsave("../documentation/images/fig_11_7.8.2_Totat Ride Distribution in Different 
       Part-of-the-Day for 2-type of Users.png", 
       plot = plt, width = 6, height = 2.5)

# Display the plot
print(plt)
```


###### Summary: 
1. Most of the ride recorded in the Afternoon and evening time (between 12PM to 9PM) 
2. Afternoon time is the pick time for both type of members. Around 36% of one-time user rides in the afternoon while its 32% for subscriber.
3. 29% of the both types of riders prefer evening time.
4. 27% of the subscribers are morning riders whiles the percentage is a bit lower for one-time users.


##### 7.8.3. Analyzing Riding Patterns by Part-of-the-Day: Weekdays vs. Weekends for Different Types of Users

```{r fig.width=10}
# getting the sum grouped by weekend status, part of the day and membership type
tab_tsc <- (df %>% count(weekend_status, part_of_day, member_casual))


tab_tsc <- tab_tsc %>%
  group_by(weekend_status, member_casual) %>%
  mutate(Percentage = n / sum(n) * 100) %>% 
  arrange(member_casual)

tab_tsc$part_of_day <- factor(tab_tsc$part_of_day, levels = custom_part_of_day)


## order the data
tab_tsc$weekend_status <- factor(tab_tsc$weekend_status, levels = c("weekdays", "weekend"))


plt <- ggplot(tab_tsc, aes(x = "", y = Percentage, fill = part_of_day)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_grid(weekend_status ~ member_casual, switch = "both") +
  theme_void() +
  theme(plot.title = element_text(hjust = 1)) + 
  geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5)) + 
  labs(fill = "Part of the Day") +
  ggtitle("Ride Distribution by Part-of-Day: Day Type & Membership")

ggsave("../documentation/images/fig_12_7.8.3_Visualizing Ride Distribution by Part-of-the-Day:
       Weekday-Weekend & Membership Type.png", 
       plot = plt, width = 6.75, height = 4)

# Display the plot
print(plt)
```


###### Summary:
1. For both types of user, the percentage of morning and night time rides is pretty much unchanged across weekdays and weekend.
2. There are sharp increase in the percentage of Afternoon rides for both types of users during the weekends. 
3. To adjust the increase in afternoon there are similar drop in the evening rides which is 10% for one-time users and 7% for subscribers. 



#### 7.9. Analyze the Riding Pattern for Diffrent Ridealbe Type


##### 7.9.1. Visulaize total Ride Distribution in Different Type of Bike

```{r fig.width=9}
# get counts of each type with respect to rideable type
tab_rt <- (df %>% count(rideable_type))

# calculate the %
tab_rt$percentage <- round((tab_rt$n / sum(tab_rt$n) * 100), 2)

tab_rt$percentage <- paste(tab_rt$percentage, "%", sep = "")


custom_colors <- c("#FF9999", "#66CC66", "#9999FF")

# plot the analysis
plt <- ggplot(tab_rt, aes(x = rideable_type, y = n, fill = rideable_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percentage), vjust = -0.5) +
  labs(x = "Bike Type", y = "Total Number of Ride") +
  ggtitle("Total Number of Ride for Different Type of Bike") +
  scale_y_continuous(labels = NULL) +
  theme(axis.text.x = element_blank())


# Customize the legend
legend_labels <-  paste(tab_rt$rideable_type, "-", tab_rt$n)  # Create legend labels
plt <- plt + scale_fill_manual(values = custom_colors, labels = legend_labels, name = "Bike Type with Count")

ggsave("../documentation/images/fig_13_7.9.1_Total Number of Ride for Different Type of Bike.png", 
       plot = plt, width = 7, height = 5)

# Display the plot
print(plt)
```


###### Summary:
1. Electric bikes are mostly used which is more than 51%. 
2. Classic bikes also used a lot and the overall percentage is almost 46%.
3. The least used bike type is docker. Only 3% rides recorded as docked bike.


##### 7.9.2. Visualize the Riding Pattern across Membership Type

```{r fig.width=9}
# get counts of each type with respect to rideable type
tab_rt_mc <- (df %>% count(rideable_type, member_casual))

# calculate the group percentage
tab_rt_mc <- tab_rt_mc %>%
  group_by(member_casual) %>%
  mutate(Percentage = n / sum(n) * 100) %>% 
  arrange(member_casual)


# plot the analysis
plt <- ggplot(tab_rt_mc, aes(x = "", y = Percentage, fill = rideable_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "rideable_type") +
  theme_void() +
  facet_wrap(~ member_casual, strip.position = "bottom") +
  labs(fill = "Bike Type") +
  geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5)) + 
  ggtitle("Ride Distribution in Different Bike type for 2-Types of Member")

ggsave("../documentation/images/fig_14_7.9.2_Total Ride Distribution in Different 
       Bike type for 2-Types of Member.png", 
       plot = plt, width = 5, height = 2.5)

# Display the plot
print(plt)
```


###### Summary:
1. Subscribers only used classic and electric bike and the ratio is almost same 51-49.
2. Within the one-time user category, electric bikes emerge as the dominant choice, constituting a substantial 54% of the total rides. 
3. Further investigation into the one-time user group reveals that 38% opt for classic bikes, showcasing a diverse range of preferences. Additionally, a noteworthy 8% of users in this category opt for docked bikes.



#### 7.10. Location-based analysis


##### 7.10.1. Get the Number of Unique Start Station Name

```{r}
# get most popular start station
st_len <- length(unique(start_station_name))

cat(paste0("Total number of unique start station is: ", st_len))
```


##### 7.10.2. Get the Number of Unique End Station Name

```{r}
# get most popular start station
ed_len <- length(unique(end_station_name))

cat(paste0("Total number of unique end station is: ", ed_len))
```


##### 7.10.3. Analyze Top-10 Unique Routes

```{r}
# get most popular start station
top_route_df <- as.data.frame(table(start_station_name, end_station_name, is_round_trip)) %>% 
  filter(start_station_name != "" & end_station_name != "") %>% 
  arrange(desc(Freq)) %>% 
  head(10)

# add a new columns which contains the route names
top_route_df$Routes <- paste(top_route_df$start_station_name, top_route_df$end_station_name, sep = " TO ")


# delete the start and end station names
top_route_df <- subset(top_route_df, select = -c(start_station_name, end_station_name))

# swap the position of the route names and freq
top_route_df <- top_route_df[c(3, 1, 2)]
top_route_df
```


##### 7.10.4. Plot the Unique Routes

```{r fig.width=12}
# make the freq numeric
top_route_df$Freq <- as.numeric(top_route_df$Freq)

# make the routes names as character from vector
top_route_df$Routes <- as.character(top_route_df$Routes)

add_newline <- function(route) {
  if (nchar(route) > 30 && grepl("TO", route)) {
    route <- gsub("TO", "TO\n", route)
  }
  return(route)
}

# Apply the function to the 'Routes' column
top_route_df$Routes <- sapply(top_route_df$Routes, add_newline)

# order the routes accordingly their counts
top_route_df$Routes <- factor(top_route_df$Routes, levels = top_route_df$Routes[order(-top_route_df$Freq)])

my_colors <- c("#594E6D", "#6A5E82", "#7C6D98", "#948BA4", "#998EAD", "#9D91B5", "#A294BE", "#A796C7", "#AC99D0", "#B19CD9")

plt <- ggplot(top_route_df, aes(x = Routes , y = Freq, fill = Routes)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.3, size = 4.5) +
  ylab("Number of Rides") +
  xlab("Routes Name") +
  ggtitle("Top-10 Unique Routes and Their Counts") +
  scale_y_continuous(labels = NULL) +
  theme(
        axis.text.x = element_blank(),  
        axis.ticks = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.margin = unit(c(1, 0, 1, 1), "cm"), 
        legend.key.height = unit(1.2, "cm")) + 
  labs(fill = "Routes Name") +
  scale_fill_manual(values = my_colors)

ggsave("../documentation/images/fig_15_7.10.4_Top-10 Unique Routes and Their Counts.png", 
       plot = plt, width = 8.75, height = 5.25)

# Display the plot
print(plt)
```


```{r}
num_round_t10_trip <- sum(top_route_df$is_round_trip == "round trip")

cat(paste0("Number of round trip in top-10 routes is: ", num_round_t10_trip))
```


###### Summary:
1. Highest number of ride that follow the route is "Streeter Dr & Grand Ave TO Streeter Dr & Grand Ave" which is **12.2K**.
2. From the graph it's clear that, some of them has same start and end station. So, I check further. 
3. There are 5 routes out of 10 routes are starts and ends at same stations. Meaning 50% of top-10 routes are consider a round trip.


##### 7.10.5. Analyze Top-Routes for Different User-type

###### 7.10.5.1. Function to Calculate Top-10 Unique Routes Different User-type

```{r}
# function for getting unique routes for each type of users
unique_routes <- function(user_type){
  # get most popular start station
  top_tmp_route_df <- as.data.frame(table(start_station_name, end_station_name, member_casual, is_round_trip)) %>% 
    filter(member_casual == user_type) %>% 
    filter(start_station_name != "" & end_station_name != "") %>%
    arrange(desc(Freq)) %>% 
    head(10)
  
  # add a new columns which contains the route names
  top_tmp_route_df$Routes <- paste(top_tmp_route_df$start_station_name, top_tmp_route_df$end_station_name, sep = " TO ")
  
  # delete the start and end station names
  top_tmp_route_df <- subset(top_tmp_route_df, select = -c(start_station_name, end_station_name))
  
  # swap the position of the route names and freq
  top_tmp_route_df <- top_tmp_route_df[c(4, 1, 2, 3)]
  
  return(top_tmp_route_df) 
}
```


###### 7.10.5.2. Fetching Top-10 Routes for One-Time Users

```{r}
# call the function
top_ot_route_df <- unique_routes("one-time users")

print(top_ot_route_df)
```


###### 7.10.5.3. Fetching Top-10 Routes for Subscribers

```{r}
top_ss_route_df <- unique_routes("subscribers")

print(top_ss_route_df)
```


###### 7.10.5.4. Calculate the Precentage of round trip in Different Type of Users

```{r}
# one-time user's total round trip
ot_t10_trip <- sum(top_ot_route_df$is_round_trip == "round trip")

# subscribers total round trip
ss_t10_trip <- sum(top_ss_route_df$is_round_trip == "round trip")

msg <- "Number of round trip in top-10 routes is: "

cat(paste0(msg, ot_t10_trip, " for One-time users\n", msg, ss_t10_trip, " for subscribers"))
```


###### 7.10.5.5. Fetch Common Routes from top-10 Routes for both type of Users

```{r}
common_routes <- intersect(top_ot_route_df$Routes, top_ss_route_df$Routes)

cat(paste0("Total Common Routes are: ", length(common_routes)))
```


###### 7.10.5.6. Function for Calculating Common Routes from Top-n Routes

```{r}
common_routes_counts <- function(n=10) {
  
  all_tmp_route_df <- as.data.frame(table(start_station_name, end_station_name, member_casual)) %>% 
  filter(start_station_name != "" & end_station_name != "") %>%
  arrange(desc(Freq))
  
  # add a new columns which contains the route names
  all_tmp_route_df$Routes <- paste(all_tmp_route_df$start_station_name, all_tmp_route_df$end_station_name, sep = " TO ")
  
  
  # delete the start and end station names
  all_tmp_route_df <- subset(all_tmp_route_df, select = -c(start_station_name, end_station_name))
  
  # swap the position of the route names and freq
  all_tmp_route_df <- all_tmp_route_df[c(3, 1, 2)]
  
  all_ot_routes <- all_tmp_route_df %>% 
  filter(member_casual == "one-time users") %>% 
  head(n)
  
  all_ss_routes <- all_tmp_route_df %>% 
  filter(member_casual == "subscribers")%>% 
  head(n)
  
  all_ss_routes
  
  all_common_routes <- intersect(all_ot_routes$Routes, all_ss_routes$Routes)
  
  return(length(all_common_routes))
}
```



###### 7.10.5.7. Analyze Common Routes from top-n Routes for both type of Users

```{r}
# get count for n=50
msg <- " routes are common from "

t_50_cnt <- sprintf("%-30s", paste0(common_routes_counts(50), msg, " 50 routes"))
t_100_cnt <- sprintf("%-30s", paste0(common_routes_counts(100), msg, " 100 routes"))
t_500_cnt <- sprintf("%-30s", paste0(common_routes_counts(500), msg, " 500 routes"))
t_1000_cnt <- sprintf("%-30s", paste0(common_routes_counts(1000), msg, " 1000 routes"))

cat(t_50_cnt, t_100_cnt, t_500_cnt, t_1000_cnt, sep = "\n")
```


###### Summary:
1. The route "Streeter Dr & Grand Ave TO Streeter Dr & Grand Ave" is the most common routes for One-time Users.
2. "Ellis Ave & 60th St TO University Ave & 57th St" route is the most common among subscribers.
3. 7 out of 10 trip are consider as round trip for one-time users where the count is 0 for subscribers. 
4. Out of top-10 routes for both groups there are no common routes.
5. 5 out of 50 top routes are common between the two groups.
6. 22 out of 100 top routes are common between the two groups.


##### 7.10.6. Riding Pattern of Membership Type on Different Trip Type

```{r fig.width=9}
tab_rtd <- df %>% 
  filter(start_station_name != "") %>% 
  filter(end_station_name != "") %>% 
  count(is_round_trip) 

tab_rtd$Percentage <- round((as.numeric(tab_rtd$n) /
                               as.numeric(sum(tab_rtd$n)) * 100), 2)

# plot the visualizations
plt1 <- ggplot(tab_rtd, aes(x = "", y = Percentage, fill = is_round_trip)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "is_round_trip") +
  theme_void() +
  geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5)) +
  ggtitle("Total Ride Distribution") +
  theme(legend.position = "none")


# get the count of round trip/one-way trip membership-wise
tab_round_trip <- df %>% 
  filter(start_station_name != "") %>% 
  filter(end_station_name != "") %>% 
  count(is_round_trip, member_casual) 


# calculate the percentage
tab_round_trip <- tab_round_trip %>%
  group_by(member_casual) %>%
  mutate(Percentage = n / sum(n) * 100) %>% 
  arrange(member_casual)


# plot the visualizations
plt2 <- ggplot(tab_round_trip, aes(x = "", y = Percentage, fill = is_round_trip)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "is_round_trip") +
  theme_void() +
  facet_wrap(~ member_casual, strip.position = "bottom") +
  labs(fill = "Trip Type") +
  geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5)) +
  ggtitle("Ride Distribution in for Different types of Members")


combined_plots <- plot_grid(plt1, plt2, ncol = 2, rel_widths = c(0.3, 0.7))

ggsave("../documentation/images/fig_16_7.10.6_Top-10 Unique Routes and Their Counts.png", 
       plot = combined_plots, width = 6.5, height = 2.5)

# Display the plot
print(combined_plots)
```


###### Summary:
1. One-time users rides 10% round trips and 90% one-way trips.
2. Subscribers rides only 5% round trips and 95% one-way trips.
