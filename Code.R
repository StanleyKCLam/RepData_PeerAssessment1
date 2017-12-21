# Clear the workspace
rm(list=ls())

# Load ggplot2
library(ggplot2)


## 1 Loading and preprocessing the data

# Read "activity.csv"" into variable activity
unzip("activity.zip")
activity <- read.csv("activity.csv")

# Show first 6 lines, summary & str of activity
head(activity)
summary(activity)
str(activity)

# Time interval is 5 mins, so each day should have (60/5)*24 = 288 readings, check if correct. Yes, from results below all 61 days have 288 readings
length(summary(activity$date))
summary(activity$date)

## 2 What is mean total number of steps taken per day?

# Calculate the total number of steps taken per day
dailySteps<-aggregate(steps~date,data=activity,sum,na.rm=TRUE)
ggplot(data=dailySteps, aes(dailySteps$steps))+
        geom_histogram(breaks=seq(from=0, to=25000, by=2500), col="red",aes(fill=..x..))+
        xlab("Steps in one day")+
        ylab("Number of days")

# Calculate and report the mean and median of the total number of steps taken per day
mean(dailySteps$steps, na.rm=T)
median(dailySteps$steps, na.rm=T)

## 3. What is the average daily activity pattern?

# Find the mean number of steps per each time interval across all days
meanSteps <- aggregate(activity$steps,
                       list(interval = as.numeric(as.character(activity$interval))),
                       FUN = "mean", na.rm=T)

# Add column names
names(meanSteps) <- c("interval", "meansteps")

# Plot the graph
ggplot(data=meanSteps, aes(x=interval, y=meansteps))+
        geom_line(col=4)+
        ylab("Mean number of steps in each interval across all days")

# Returning the row with maximum average number of steps
meanSteps[which.max(meanSteps$meansteps),]

## 4. Imputing missing values

# Calculate and report the total number of missing values in the dataset
nrow(activity[is.na(activity$steps),])

# Imputing NA's with mean of steps across all days, creating a new
imputed_act <- activity
for (i in which(is.na(activity$steps))) {
        imputed_act[i,"steps"] <- meanSteps[meanSteps$interval==imputed_act[i,"interval"],2]
}

# Make a histogram of the total number of steps taken each day, imputing NA's
dailySteps_Imp <- aggregate(steps~date,data=imputed_act,sum)
ggplot(data=dailySteps_Imp, aes(dailySteps_Imp$steps))+
        geom_histogram(breaks=seq(from=0, to=25000, by=2500), col="red",aes(fill=..x..))+
        xlab("Steps in one day")+
        ylab("Number of days")

# Calculate the mean and median of the total number of steps taken each day, imputing NA's
mean(dailySteps_Imp$steps)
median(dailySteps_Imp$steps)


## 5. Are there differences in activity patterns between weekdays and weekends?

imp_act_f <- cbind(imputed_act, day = as.factor(weekdays(as.Date(imputed_act$date))))
wkend <- c("Saturday","Sunday")
wkday <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
imp_act_f$daytype <- factor(ifelse(imp_act_f$day %in% wkend, 1, 2))
levels(imp_act_f$daytype) <- c("Weekends","Weekdays")

# Find the mean number of steps per each time interval by day type across all days
meanStepsT <- aggregate(imp_act_f$steps,
                        list(interval = as.numeric(as.character(activity$interval)), daytype=imp_act_f$daytype),
                        FUN = "mean")

# Add column names
names(meanStepsT) <- c("interval", "daytype", "meansteps")

# Plot the graph
ggplot(data=meanStepsT, aes(x=interval, y=meansteps))+
        geom_line(col=4)+
        facet_grid(daytype~.)+
        ylab("Mean number of steps in each interval across all days")