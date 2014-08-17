Reproducible Research - Peer Assessment 1
========================================================

## Loading and preprocessing the data
#### 1. Load the data (i.e. read.csv())

```r
A <- read.csv('~/Desktop/DataScience/activity.csv', colClasses=c(NA,"Date",NA))
```
#### 2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
C <- A[complete.cases(A),]
```
## What is mean total number of steps taken per day?
#### 1. Make a histogram of the total number of steps taken each day

```r
hist(aggregate( C$steps ~ C$date, FUN=sum)[,2], main="Histogram of total number of steps each day", xlab="Steps by Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 
#### 2. Calculate and report the mean and median total number of steps taken per day

```r
mean(aggregate( C$steps ~ C$date, FUN=sum)[,2])
```

```
## [1] 10766
```

```r
median(aggregate( C$steps ~ C$date, FUN=sum)[,2])
```

```
## [1] 10765
```
## What is the average daily activity pattern?
#### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
I<-aggregate( C$steps ~ C$interval , FUN = mean)
names(I) <- c("Interval", "AvgStepsByInterval")
plot(I$Interval, I$AvgStepsByInterval, type="l", main="Average Steps Taken by Interval Across All Days", xlab="Interval", ylab="Average Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 
#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
I[I[2] == max(I[2])][1]
```

```
## [1] 835
```

## Imputing missing values
#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(A) - nrow(C)
```

```
## [1] 2304
```
#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
The strategy is to replace all NA step values with the average across all days (mean) for the 5-minute interval.

```r
library(plyr)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
```
#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
F <- ddply(A, ~ interval, transform, steps = impute.mean(steps))
```
#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
hist(aggregate( F$steps ~ F$date, FUN=sum)[,2], main="Histogram of total number of steps each day", xlab="Steps by Day")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
mean(aggregate( F$steps ~ F$date, FUN=sum)[,2])
```

```
## [1] 10766
```

```r
median(aggregate( F$steps ~ F$date, FUN=sum)[,2])
```

```
## [1] 10766
```
The mean value does not differ, however the median value is now closer to the mean due to the impact of imputing the mean values on on the missing data.
## Are there differences in activity patterns between weekdays and weekends?
#### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
F$we <- NA
F <- within(F, 
{
we[weekdays(date) == 'Sunday'] <- 'weekend'
we[weekdays(date) == 'Monday'] <- 'weekday'
we[weekdays(date) == 'Tuesday'] <- 'weekday'
we[weekdays(date) == 'Wednesday'] <- 'weekday'
we[weekdays(date) == 'Thursday'] <- 'weekday'
we[weekdays(date) == 'Friday'] <- 'weekday'
we[weekdays(date) == 'Saturday'] <- 'weekend'
})
```
#### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
E <- subset(F, we == "weekend")
D <- subset(F, we == "weekday")
WE <-aggregate( E$steps ~ E$interval , FUN = mean)
WD <-aggregate( D$steps ~ D$interval , FUN = mean)
names(WE) <- c("Interval", "AvgStepsByInterval")
names(WD) <- c("Interval", "AvgStepsByInterval")

par(mfrow=c(2,1))
plot(WD$Interval, WD$AvgStepsByInterval, type="l", main="Weekday", xlab="Interval", ylab="Average Steps")
plot(WE$Interval, WE$AvgStepsByInterval, type="l", main="Weekend", xlab="Interval", ylab="Average Steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

