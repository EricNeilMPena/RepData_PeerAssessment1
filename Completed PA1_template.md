---
title: 'Peer-graded Assignment: Course Project'
author: 'Eric Neil Pena'
date: 'May 11, 2019'
output: html_document
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Loading and Preprocessing the data
Install Required libraries
```{r, results='hide', message=FALSE, warning=FALSE}
chooseCRANmirror(graphics=FALSE, ind=1)

install.packages('lubridate')
install.packages('dplyr')
install.packages('lattice')
```

Load the required libraries
```{r  results='hide', message=FALSE, warning=FALSE}
chooseCRANmirror(graphics=FALSE, ind=1)

library(lubridate)
library(dplyr)
library(lattice)
```

download and unzip the file to get the actual csv file

```{r load data}
url <- "https://github.com/EricNeilMPena/RepData_PeerAssessment1/raw/master/activity.zip"
destination <- "dataset.zip"
download.file(url, destination)
```

```{r unzip, cache=TRUE}
unzip(destination)
activity <- read.csv("activity.csv")
```

Get the Structure of the data by typing the following commands
```{r}
str(activity[which(!is.na(activity$steps)), ])
names(activity)
summary(activity[which(!is.na(activity$steps)), ])
head(activity[which(!is.na(activity$steps)), ])
```

Transform the data to be ready for analysis
```{r}
activity$date <- ymd(activity$date)
```
#What is the mean total number of steps taken per day
Calculate the total number of steps taken per day

```{r}
total_steps_per_day <- activity %>% group_by(date) %>% summarise(total_steps_per_day=sum(steps, na.rm=TRUE))
total_steps_per_day
```
Histogram of the total number of steps taken each day

```{r}

hist(total_steps_per_day$total_steps_per_day,
     col="blue",
     xlab="Steps",
     ylab="Frequency",
     main="Total number of Steps taken per day")
```
Calculate and report the mean and median of the total number of steps taken per day
```{r summary}
total_steps_mean <- mean(total_steps_per_day$total_steps_per_day,na.rm=TRUE)
total_steps_mean
total_steps_median <- median(total_steps_per_day$total_steps_per_day,na.rm=TRUE)
total_steps_median
```

#What is the average daily pattern
1.) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
average_daily_pattern <- activity %>% group_by(interval) %>% summarise(average=mean(steps,na.rm=TRUE))
plot(x = 1:nrow(average_daily_pattern),y = average_daily_pattern$average,type = "l",
     col = "red",xlab="Intervals", 
     ylab = "Average of the interval across all days")
```

2.) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
max_number_of_steps_interval <- filter(average_daily_pattern, average==max(average))
max_number_of_steps_interval
```

#Imputing Missing values
1.) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
sum(is.na(activity$steps))
```

2.) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The strategy that I did is to fill in the datapoint with missing values with the mean of the steps taken across all dates

3.) Create a new dataset that is equal to the original dataset but with the missing data filled in

```{r}
impute_mean <- activity
with_na <- is.na(impute_mean$steps)
impute_mean$steps[with_na] <- mean(impute_mean$steps, na.rm = TRUE)
```

4.) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
without_na <- activity[with_na == FALSE,]

steps_daily <- tapply(without_na$steps, without_na$date, sum, na.rm=TRUE, simplify=T)

hist(x=steps_daily,
     col="blue",
     xlab="daily steps per day",
     breaks = 15,
     ylab="frequency",
     main="Histogram of daily total steps without missing value")
```

```{r}
mean(steps_daily)
```

```{r}
median(steps_daily)
```
the average and the median of total steps are now equal in contrast with the original dataset after NAs have been removed.

#Are there differences in activity patterns between weekdays and weekends?
1.) Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
activity_new <- mutate(without_na, day = ifelse(weekdays(without_na$date) == "Saturday" | weekdays(without_na$date) == "Sunday", "weekend", "weekday"))
activity_new$day<- as.factor(activity_new$day)
head(activity_new)
str(activity_new)
```

2.) Make a panel plot containing a time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r}

without_na_new <- aggregate(steps ~ day+interval, data=activity_new, FUN=mean)
xyplot(steps ~ interval | factor(day),
       layout = c(1, 2),
       xlab="Interval",
       ylab="Number of steps",
       type="l",
       lty=1,
       data=without_na_new)
```
