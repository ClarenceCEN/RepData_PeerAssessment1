---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv")
Sys.setlocale("LC_TIME","English")
```

```
## [1] "English_United States.1252"
```

```r
locale_original <- Sys.getlocale( category = "LC_TIME" )
library(ggplot2)
```


## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day

```r
steps_date_sum <- tapply(activity$steps, activity$date, sum)
hist(steps_date_sum, xlab = "steps per day", main =" Histogram of steps per day", breaks = 10)
```

![plot of chunk unnamed-chunk-41](figure/unnamed-chunk-41-1.png)

2. Calculate and report the mean and median total number of steps taken per day

```r
steps_mean <- mean(steps_date_sum, na.rm = TRUE)
steps_median <- median(steps_date_sum, na.rm = TRUE)

mean_c <- paste0("The mean of the total number of steps taken per day is ", steps_mean, ".")
median_c <- paste0("The median of the total number of steps taken per day is ", steps_median, ".")

print(mean_c);print(median_c)
```

```
## [1] "The mean of the total number of steps taken per day is 10766.1886792453."
```

```
## [1] "The median of the total number of steps taken per day is 10765."
```


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps_interval_mean <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
par(mar=c(5,4,0.5,1))
plot(steps_interval_mean, type="l", xlab = "interval", ylab = "steps")
```

![plot of chunk unnamed-chunk-43](figure/unnamed-chunk-43-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_interval <- which.max(steps_interval_mean)
print(names(max_interval))
```

```
## [1] "835"
```

```r
print("The interval of 835 - 840 contains the maximum number of steps.")
```

```
## [1] "The interval of 835 - 840 contains the maximum number of steps."
```



## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
n_na <- sum(is.na(activity$steps));print(n_na)
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
I choose to use the mean for that 5-minute interval.

```r
activity.no_na <- activity
activity.no_na <- as.data.frame(activity.no_na)
for(i in 1:nrow(activity)){
  if(is.na(activity.no_na$steps[i])==TRUE){
    intv <- activity.no_na$interval[i]
    intv <- as.character(intv)
    activity.no_na$steps[i] <- steps_interval_mean[intv]
  }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps_date_sum.no_na <- tapply(activity.no_na$steps, activity.no_na$date, sum)
hist(steps_date_sum.no_na, xlab = "steps per day", main =" Histogram of steps per day", breaks = 10)
```

![plot of chunk unnamed-chunk-47](figure/unnamed-chunk-47-1.png)

```r
steps_mean.no_na <- mean(steps_date_sum.no_na);print(steps_mean.no_na)
```

```
## [1] 10766.19
```

```r
mean_diff <- steps_mean - steps_mean.no_na; print(mean_diff)
```

```
## [1] 0
```

```r
steps_median.no_na <- median(steps_date_sum.no_na);print(steps_median.no_na)
```

```
## [1] 10766.19
```

```r
median_diff <- steps_median - steps_median.no_na; print(median_diff)
```

```
## [1] -1.188679
```

```r
total_diff <- sum(steps_date_sum, na.rm = TRUE) - sum(steps_date_sum.no_na); print(total_diff)
```

```
## [1] -86129.51
```


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

```r
days <- weekdays(as.Date(activity$date))
activity.no_na$day <- "weekdays"
weekends_ind <- which((days == "Saturday")|(days == "Sunday"))
activity.no_na$day[weekends_ind] <- "weekends"
activity.no_na <- transform(activity.no_na, day = factor(day))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

```r
library(dplyr);library(lattice)
```

```r
activity_day <- aggregate(steps~interval+day, activity.no_na, sum)

xyplot(steps~interval|day, data = activity_day, layout = c(1,2), type="l")
```

![plot of chunk unnamed-chunk-50](figure/unnamed-chunk-50-1.png)
