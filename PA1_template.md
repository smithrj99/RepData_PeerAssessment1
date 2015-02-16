---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
```
## What is mean total number of steps taken per day?

```r
# numbers >= 10^5 will be denoted in scientific notation,
# and rounded to 2 digits
options(scipen = 1, digits = 2)

# Get the total of steps per day, then the mean/median of those
totalStepsPerDay <- aggregate(steps ~ date, data = activity, FUN = sum )
meanStepsPerDay <- mean(totalStepsPerDay$steps)
medianStepsPerDay <- median(totalStepsPerDay$steps)

#Display the histogram of total steps/day
hist(totalStepsPerDay$steps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

The mean total number of steps was 10766.19, while the median was 10765.

## What is the average daily activity pattern?


```r
meanStepsPerInterval <- aggregate(steps ~ interval, data = activity, FUN = mean )
plot(meanStepsPerInterval, type="l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
maxStepsInterval <- meanStepsPerInterval[meanStepsPerInterval$steps == max(meanStepsPerInterval$steps), ]
maxInverval <- maxStepsInterval[1]
maxSteps <- maxStepsInterval[2]
```

The interval at 835 has the maximum number of average steps at 206.17.


## Imputing missing values

```r
# numbers >= 10^5 will be denoted in scientific notation,
# and rounded to 2 digits
options(scipen = 1, digits = 2)

# Determine number of rows with missing values
missing <- sum(!complete.cases(activity))

# Substitute the mean number of steps for the missing values
meanStepsPerDate <- aggregate(steps ~ date, data = activity, FUN = mean )
completeActivity <- activity
for(currentDate in unique(activity[complete.cases(activity), 2]))
{
    completeActivity[!complete.cases(completeActivity) && completeActivity$date == currentDate, 1] <-
        meanStepsPerDate[meanStepsPerDate$date == currentDate, 2]  
}

# Get the total of steps per day, then the mean/median of those
totalStepsPerDay <- aggregate(steps ~ date, data = completeActivity, FUN = sum )
meanStepsPerDay <- mean(totalStepsPerDay$steps)
medianStepsPerDay <- median(totalStepsPerDay$steps)

#Display the histogram of total steps/day
hist(totalStepsPerDay$steps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

The mean total number of steps from imputed data set was 10766.19, while the median was 10765. Using the imputed values has no impact on the daily summary values.

## Are there differences in activity patterns between weekdays and weekends?


```r
library(chron)
library(lattice)

completeActivity$weekend <- "weekday"
completeActivity[is.weekend(completeActivity$date), 4] <- "weekend"

meanStepsPerIntervalWeekend <- aggregate(steps ~ interval + weekend, data = completeActivity, FUN = mean )
xyplot(steps ~ interval | factor(weekend), data = meanStepsPerIntervalWeekend, type="l", layout = c(1,2))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
