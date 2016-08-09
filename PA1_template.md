# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

1. Load the data


```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
```
2. Process the data - by aggregate and ignore missing values


```r
tSteps <- aggregate(steps ~ date, data = activity, sum, na.rm=TRUE)
```

## What is mean total number of steps taken per day?

1. Make a histogram of total number of steps taken each day


```r
hist(tSteps$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

2. Calculate and report the mean and median of total number of steps per day


```r
mean(tSteps$steps)
```

```
## [1] 10766.19
```

```r
median(tSteps$steps)
```

```
## [1] 10765
```

- **Mean**: The mean number of steps taken per day is 1.0766189\times 10^{4}

- **Median**: The median number of steps taken per day is 10765

## What is the average daily activity pattern?

1. Make a time series plot(i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsInterval<-aggregate(steps~interval,data=activity,mean,na.rm=TRUE)
plot(steps~interval,data=stepsInterval,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
stepsInterval[which.max(stepsInterval$steps),]$interval
```

```
## [1] 835
```

It is the 835th interval

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Number of missing values = 2304 .

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

- Used a strategy to fill all the missing values by mean


```r
interval2steps<-function(interval){
    stepsInterval[stepsInterval$interval==interval,]$steps
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityFilled<-activity   # Make a new dataset with the original data
count=0           # Count the number of data filled in
for(i in 1:nrow(activityFilled)){
    if(is.na(activityFilled[i,]$steps)){
        activityFilled[i,]$steps<-interval2steps(activityFilled[i,]$interval)
        count=count+1
    }
}
cat("Total ",count, "NA values were filled.\n\r")
```

```
## Total  2304 NA values were filled.
## 
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
tSteps2<-aggregate(steps~date,data=activityFilled,sum)
hist(tSteps2$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean(tSteps2$steps)
```

```
## [1] 10766.19
```

```r
median(tSteps2$steps)
```

```
## [1] 10766.19
```

- **Mean**: The mean of steps taken per day is 1.0766189\times 10^{4}

- **Median**: The median of steps taken per day is 1.0766189\times 10^{4}

5. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

- The mean value is the same as the value before imputing missing data because we put the mean value for that particular 5-min interval. The median value shows a little difference, but it depends on where the missing values are.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day


```r
activityFilled$day=ifelse(as.POSIXlt(as.Date(activityFilled$date))$wday%%6==0,
                          "weekend","weekday")
# For Sunday and Saturday : weekend, Other days : weekday 
activityFilled$day=factor(activityFilled$day,levels=c("weekday","weekend"))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
stepsInterval2=aggregate(steps~interval+day,activityFilled,mean)
library(lattice)
xyplot(steps~interval|factor(day),data=stepsInterval2,aspect=1/2,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
