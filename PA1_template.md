---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
data<-read.csv('/Users/sumeetmishra/Desktop/R_Programing/activity.csv')
#total steps per day
totalSteps<-tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
```

## What is mean total number of steps taken per day?

```r
hist(totalSteps, xlab="Total Number of Steps Taken Per Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
mean(totalSteps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(totalSteps, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
library(ggplot2)
averageSteps <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
plot<-ggplot(data=averageSteps, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("Average Number of Steps Taken")
print(plot)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
averageSteps[which.max(averageSteps$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```
## Imputing missing values

```r
NAs<- is.na(data$steps)
# Number of NA's
table(NAs)
```

```
## NAs
## FALSE  TRUE 
## 15264  2304
```

```r
data_withoutNA <- split(data, data$interval)
data_withoutNA <- lapply(data_withoutNA, function(x) {
  x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
  return(x)
})
data_withoutNA <- do.call("rbind", data_withoutNA)
row.names(data_withoutNA) <- NULL

data_withoutNA <- split(data_withoutNA, data_withoutNA$date)
df <- lapply(data_withoutNA, function(x) {
  x$steps[which(is.na(x$steps))] <- mean(x$steps, na.rm = TRUE)
  return(x)
})
data_withoutNA <- do.call("rbind", data_withoutNA)
row.names(data_withoutNA) <- NULL
head(data_withoutNA)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
totalSteps_withoutNA <- tapply(data_withoutNA$steps, data_withoutNA$date, FUN=sum)
hist(totalSteps_withoutNA, xlab="Total Number of Steps Taken Per Day without NA Values Added")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
mean(totalSteps_withoutNA)
```

```
## [1] 10766.19
```

```r
median(totalSteps_withoutNA)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
data_withoutNA$date <- as.Date(data_withoutNA$date)
data_withoutNA$day <- sapply(data_withoutNA$date, FUN=weekday.or.weekend)



averageSteps <- aggregate(steps ~ interval + day, data=data_withoutNA, mean)
ggplot(averageSteps, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
