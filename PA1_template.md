---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
activity = read.csv("activity.csv")

total_steps_per_day <- sum(activity$steps, na.rm = TRUE)
total_steps_each_day <- aggregate(steps~date, data=activity, FUN=sum, na.rm=TRUE)

hist(total_steps_each_day$steps, xlab="Total steps each day")
hist(total_steps_each_day$steps, main="Total steps each day",xlab="")

## What is mean total number of steps taken per day?
mean_steps_each_day <- mean(total_steps_each_day$steps)
median_steps_each_day <- median(total_steps_each_day$steps)

## What is the average daily activity pattern?
average_five_minutes <- aggregate(steps~interval, data=activity, FUN=mean, na.rm=TRUE)

plot(x = average_five_minutes$interval, y = average_five_minutes$steps, type = "l", xlab="Five minutes interval (Average)", ylab="")

max_steps <- max(average_five_minutes$steps)
for (i in 1:288) 
{
    if (average_five_minutes$steps[i] == max_steps)
        five_minute_interval_at_max_steps <- average_five_minutes$interval[i]
}

five_minute_interval_at_max_steps

## Imputing missing values
total_na <- 0
for (i in 1:17568)
{
    if(is.na(activity$steps[i])) 
    total_na <- total_na+1 
}

total_na

activity_without_na <- activity
for (i in 1:17568) 
{
    if(is.na(activity_without_na$steps[i]))
    { 
        five_minute_pointer <- activity_without_na$interval[i]
           for (j in 1:288)
           {
              if (average_five_minutes$interval[j] == five_minute_pointer)
                  activity_without_na$steps[i] <- average_five_minutes$steps[j] 
           }
    }
}

total_steps_each_day <- aggregate(steps~date, data=activity_without_na, FUN=sum, na.rm=TRUE)

hist(total_steps_each_day$steps, main="Total steps each day", xlab="")

total_steps_each_day_mean <- mean(total_steps_each_day$steps)
total_steps_each_day_median <- median(total_steps_each_day$steps)


## Are there differences in activity patterns between weekdays and weekends?
ctivity$date <- as.Date(activity$date)
activity_without_na$date <- as.Date(activity_without_na$date)

week <- weekdays(activity_without_na$date)

week_day <- week
for (i in 1:17568)
{
    if(week[i] == 1)
       week_day[i] <- 'weekend'
    if(week[i] == 2)
       week_day[i] <- 'weekday'
    if(week[i] == 3)
       week_day[i] <- 'weekday'
    if(week[i] == 4)
       week_day[i] <- 'weekday'
    if(week[i] == 5)
       week_day[i] <- 'weekday'
    if(week[i] == 6)
       week_day[i] <- 'weekday'
    if(week[i] == 7)
       week_day[i] <- 'weekend'
}

activity_without_na$weekday <-week_day
weekday <- grep("weekday",activity_without_na$weekday)
average_five_minutes_weekday <- aggregate(steps~interval, data=weekday_frame, FUN=mean, na.rm=TRUE)
average_five_minutes_weekend <- aggregate(steps~interval, data=weekend_frame, FUN=mean, na.rm=TRUE)
plot(x = average_five_minutes_weekday$interval, y = average_five_minutes_weekday$steps, type = "l") 
plot(x = average_five_minutes_weekend$interval, y = average_five_minutes_weekend$steps, type = "l") 