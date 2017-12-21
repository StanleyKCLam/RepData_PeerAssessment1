---
title: "Reproducible Research: Peer Assessment 1 by Stanley Lam"
output: 
  html_document:
    keep_md: true
---


## 1. Loading and preprocessing the data

#### Clear the workspace


```r
rm(list=ls())
```

#### Load ggplot2


```r
library(ggplot2)
```

#### Read "activity.csv"" into variable activity


```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
```

#### Show first 6 lines, summary & str of activity


```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

### * Time interval is 5 mins, so each day should have (60/5)x24 = 288 readings, check if correct. Yes, from results below all 61 days have 288 readings


```r
length(summary(activity$date))
```

```
## [1] 61
```

```r
summary(activity$date)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##        288        288        288        288        288        288 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##        288        288        288        288        288        288 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##        288        288        288        288        288        288 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##        288        288        288        288        288        288 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##        288        288        288        288        288        288 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##        288        288        288        288        288        288 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##        288        288        288        288        288        288 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##        288        288        288        288        288        288 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##        288        288        288        288        288        288 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##        288        288        288        288        288        288 
## 2012-11-30 
##        288
```

## 2. What is mean total number of steps taken per day?

#### Calculate the total number of steps taken per day


```r
dailySteps<-aggregate(steps~date,data=activity,sum,na.rm=TRUE)
ggplot(data=dailySteps, aes(dailySteps$steps))+
        geom_histogram(breaks=seq(from=0, to=25000, by=2500), col="red",aes(fill=..x..))+
        xlab("Steps in one day")+
        ylab("Number of days")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

#### Calculate and report the mean and median of the total number of steps taken per day


```r
mean(dailySteps$steps, na.rm=T)
```

```
## [1] 10766.19
```

```r
median(dailySteps$steps, na.rm=T)
```

```
## [1] 10765
```

### * A histogram has a continuous x-axis variable, whereas a barplot has categorical or discrete x-axis variable
### * We see that the median is slightly lower than the mean

## 3. What is the average daily activity pattern?

#### Find the mean number of steps per each time interval across all days


```r
meanSteps <- aggregate(activity$steps,
                       list(interval = as.numeric(as.character(activity$interval))),
                       FUN = "mean", na.rm=T)
```

#### Add column names


```r
names(meanSteps) <- c("interval", "meansteps")
```

#### Plot the graph


```r
ggplot(data=meanSteps, aes(x=interval, y=meansteps))+
        geom_line(col=4)+
        ylab("Mean number of steps in each interval across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

#### Returning the row with maximum average number of steps


```r
meanSteps[which.max(meanSteps$meansteps),]
```

```
##     interval meansteps
## 104      835  206.1698
```

### * We see that the maximum average number of step happens at time interval 835

## 4. Imputing missing values

#### Calculate and report the total number of missing values in the dataset


```r
nrow(activity[is.na(activity$steps),])
```

```
## [1] 2304
```

### * Imputing NA's with mean of steps across all days


```r
imputed_act <- activity
for (i in which(is.na(activity$steps))) {
        imputed_act[i,"steps"] <- meanSteps[meanSteps$interval==imputed_act[i,"interval"],2]
}
```

#### Make a histogram of the total number of steps taken each day, imputing NA's


```r
dailySteps_Imp <- aggregate(steps~date,data=imputed_act,sum)
ggplot(data=dailySteps_Imp, aes(dailySteps_Imp$steps))+
        geom_histogram(breaks=seq(from=0, to=25000, by=2500), col="red",aes(fill=..x..))+
        xlab("Steps in one day")+
        ylab("Number of days")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

#### Calculate the mean and median of the total number of steps taken each day, imputing NA's


```r
mean(dailySteps_Imp$steps)
```

```
## [1] 10766.19
```

```r
median(dailySteps_Imp$steps)
```

```
## [1] 10766.19
```

### * The mean is the same as the mean is used to impute the missing values. The median is slightly increased (by 1.19)

## 5. Are there differences in activity patterns between weekdays and weekends?


```r
imp_act_f <- cbind(imputed_act, day = as.factor(weekdays(as.Date(imputed_act$date))))
wkend <- c("Saturday","Sunday")
wkday <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
imp_act_f$daytype <- factor(ifelse(imp_act_f$day %in% wkend, 1, 2))
levels(imp_act_f$daytype) <- c("Weekends","Weekdays")
```

#### Find the mean number of steps per each time interval by day type across all days


```r
meanStepsT <- aggregate(imp_act_f$steps,
                        list(interval = as.numeric(as.character(activity$interval)), daytype=imp_act_f$daytype),
                        FUN = "mean")
```

#### Add column names


```r
names(meanStepsT) <- c("interval", "daytype", "meansteps")
```

#### Plot the graph


```r
ggplot(data=meanStepsT, aes(x=interval, y=meansteps))+
        geom_line(col=4)+
        facet_grid(daytype~.)+
        ylab("Mean number of steps in each interval across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

### * From the graphs, we can see that there are relatively fewer steps in the morning at the weekend, but more steps in the afternoon and the evening
