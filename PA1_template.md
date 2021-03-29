---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---

Loading and preprocessing the data

```r
activity = read.csv("activity.csv")
```


```r
total_steps_per_day <- sum(activity$steps, na.rm = TRUE)
total_steps_each_day <- aggregate(steps~date, data=activity, FUN=sum, na.rm=TRUE)
hist(total_steps_each_day$steps, main="Total steps each day",xlab="")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

What is mean total number of steps taken per day?

```r
mean_steps_each_day <- mean(total_steps_each_day$steps)
median_steps_each_day <- median(total_steps_each_day$steps)

mean_steps_each_day
```

```
## [1] 10766.19
```

```r
median_steps_each_day
```

```
## [1] 10765
```

What is the average daily activity pattern?

```r
average_five_minutes <- aggregate(steps~interval, data=activity, FUN=mean, na.rm=TRUE)

plot(x = average_five_minutes$interval, y = average_five_minutes$steps, type = "l", xlab="Five minutes interval (Average)", ylab="")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
max_steps <- max(average_five_minutes$steps)
for (i in 1:288) 
{
    if (average_five_minutes$steps[i] == max_steps)
        five_minute_interval_at_max_steps <- average_five_minutes$interval[i]
}

five_minute_interval_at_max_steps
```

```
## [1] 835
```
Imputing missing values

```r
total_na <- 0
for (i in 1:17568)
{
    if(is.na(activity$steps[i])) 
    total_na <- total_na+1 
}

total_na
```

```
## [1] 2304
```

```r
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
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
total_steps_each_day_mean <- mean(total_steps_each_day$steps)
total_steps_each_day_median <- median(total_steps_each_day$steps)
```

Are there differences in activity patterns between weekdays and weekends?

```r
activity$date <- as.Date(activity$date)
activity_without_na$date <- as.Date(activity_without_na$date)
week <- weekdays(activity_without_na$date)

week_days <- week
for (i in 1:17568)
{
    if(week[i] == 1)
       week_days[i] <- 'weekend'
    if(week[i] == 2)
       week_days[i] <- 'weekday'
    if(week[i] == 3)
       week_days[i] <- 'weekday'
    if(week[i] == 4)
       week_days[i] <- 'weekday'
    if(week[i] == 5)
       week_days[i] <- 'weekday'
    if(week[i] == 6)
       week_days[i] <- 'weekday'
    if(week[i] == 7)
       week_days[i] <- 'weekend'
}

activity_without_na$weekdays <-week_days
weekdays <- grep("weekdays",activity_without_na$weekdays)
weekdays_frame <- activity_without_na[weekdays,]

plot(x = average_five_minutes$interval, y = average_five_minutes$steps, type = "l") 
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
