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
totalSteps<-aggregate(data$steps~data$date, FUN=sum,)
colnames(totalSteps)<- c("Date", "Steps")

data$day <- weekdays(as.Date(data$date))
data$DateTime<- as.POSIXct(data$date, format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

```r
hist(totalSteps$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
as.integer(mean(totalSteps$Steps, na.rm=TRUE))
```

```
## [1] 10766
```

```r
as.integer(median(totalSteps$Steps, na.rm=TRUE))
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
library(ggplot2)
library(plyr)
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

```r
clean <- data[!is.na(data$steps),]
avgTable <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))
```
## Imputing missing values

```r
NAs<- is.na(data$steps)
## Create dataset with all NAs for substitution
NAs<- data[is.na(data$steps),]
## Merge NA data with average weekday interval for substitution
newdata<-merge(NAs, avgTable, by=c("interval", "day"))

newdata2<- newdata[,c(6,4,1,2,5)]
colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")

##Merge the NA averages and non NA data together
data_withoutNA<- rbind(clean, newdata2)


totalSteps_withoutNA <- aggregate(data_withoutNA$steps~data_withoutNA$date, FUN=sum)
colnames(totalSteps_withoutNA)<- c("Date", "Steps")

as.integer(mean(totalSteps_withoutNA$Steps))
```

```
## [1] 10821
```

```r
as.integer(median(totalSteps_withoutNA$Steps))
```

```
## [1] 11015
```

```r
## Creating the histogram of total steps per day, categorized by data set to show impact


hist(totalSteps_withoutNA$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Black")
hist(totalSteps$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Grey", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
mean(totalSteps_withoutNA)
```

```
## Warning in mean.default(totalSteps_withoutNA): argument is not numeric or
## logical: returning NA
```

```
## [1] NA
```

```r
median(totalSteps_withoutNA)
```

```
## Error in median.default(totalSteps_withoutNA): need numeric data
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
