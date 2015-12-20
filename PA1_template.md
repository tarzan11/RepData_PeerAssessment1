# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
I used dplyr and ggplot 2 (in addition to base functionality) for this assignment:

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

Load the data ... 


```r
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day

```r
daily <- activity %>% group_by(date) %>% summarise(sumdaily=sum(steps))
```

Make a histogram of the total number of steps taken each day

```r
hist(daily$sumdaily)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

Calculate and report the mean and median of the total number of steps taken per day

```r
mean(daily$sumdaily, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(daily$sumdaily, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
intervals <- activity %>% group_by(interval) %>% summarise(avgdailyint=mean(steps, na.rm=TRUE))

plot(intervals$interval,intervals$avgdailyint,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
filter(intervals,avgdailyint==max(intervals$avgdailyint))
```

```
## Source: local data frame [1 x 2]
## 
##   interval avgdailyint
##      (int)       (dbl)
## 1      835    206.1698
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
count(activity, is.na(steps)==TRUE)
```

```
## Source: local data frame [2 x 2]
## 
##   is.na(steps) == TRUE     n
##                  (lgl) (int)
## 1                FALSE 15264
## 2                 TRUE  2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

For this, I used the average daily steps for that specific interval.

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityX <- left_join(activity,intervals,by="interval")
activityX <- activityX %>% mutate(steps=ifelse(is.na(steps),avgdailyint,steps)) %>% select(steps,date,interval)
dailyX <- activityX %>% group_by(date) %>% summarise(sumdaily=sum(steps))
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
hist(dailyX$sumdaily)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
mean(dailyX$sumdaily)
```

```
## [1] 10766.19
```

```r
median(dailyX$sumdaily)
```

```
## [1] 10766.19
```

The mean stayed the same while the median got close to the mean.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activityX <- activityX %>% mutate(weekend = ifelse(weekdays(as.Date(activityX$date)) %in% c("Saturday" ,"Sunday"),"weekend","weekday"))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
intervalsX <- activityX %>%  group_by(interval,weekend) %>% summarise(avgdailyint=mean(steps))

intervalsX$weekend <- factor(intervalsX$weekend, levels=c("weekend","weekday"))

p <- ggplot(data = intervalsX, aes(x = interval, y = avgdailyint)) + geom_line(colour="blue") 
p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), axis.line = element_line(colour = "black"))+ ylab("Number of Steps")
p + facet_wrap(~weekend, ncol=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
