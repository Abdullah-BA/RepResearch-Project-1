library(dplyr)
library(lubridate)
library(ggplot2)
library(httpuv)

# Qns 1: Loading and preprocessing the data

# Load file and have a quick look
activity <- read.csv("activity.csv", header=TRUE, na.strings = "NA")

# Clean up date class
activity$date <- ymd(activity$date)

# Remove NA
activity1 <- na.omit(activity)

# Make new variables 
activity$monthly <- as.numeric(format(activity$date, "%m"))
activity$daily <- as.numeric(format(activity$date, "%d"))

# Quick check
summary(activity1)

str(activity1)

head(activity1)

tail(activity1)

# Qns 2: What is mean total number of steps taken per day?

# Summarize data for ggplot
activity2 <- summarize(group_by(activity1,date),daily.step=sum(steps))
mean.activity <- as.integer(mean(activity2$daily.step))
median.activity <- as.integer(median(activity2$daily.step))

# Plot histogram
plot.steps.day <- ggplot(activity2, aes(x=daily.step)) + 
  geom_histogram(binwidth = 1000, aes(y=..count.., fill=..count..)) + 
  geom_vline(xintercept=mean.activity, colour="red", linetype="dashed", size=1) +
  geom_vline(xintercept=median.activity, colour="green" , linetype="dotted", size=1) +
  labs(title="Histogram of Number of Steps taken each day", y="Frequency", x="Daily Steps") 
plot.steps.day

# Mean total number of steps taken per day
mean.activity

# Median total number of steps taken per day
median.activity

# Qns 3: What is the average daily activity pattern?

# Prepare data for ggplot
activity3 <- activity1 %>% group_by(interval) %>% summarize(mean.step=mean(steps))

# Plot average number of steps by 5-minute interval
plot.step.interval <- ggplot(activity3, aes(x=interval,y=mean.step)) + 
  geom_line(color="red") + 
  labs(title="Average Number of Steps Taken vs 5-min Interval", y="Average Number of Steps", x="5-min Interval Times Series")
plot.step.interval

optimal <- which.max(activity3$mean.step)
optimal.step <- activity3$interval[optimal]
sprintf("Maximum number of steps is coming from %gth 5-min interval", optimal.step)

# Qns 4: Imputing missing values

# Total number of missing values in the dataset
sum(is.na(activity))

impute.activity <- activity
impute.activity$steps[is.na(impute.activity$steps)] <- mean(impute.activity$steps,na.rm=TRUE)
impute.activity$steps <- as.numeric(impute.activity$steps)
impute.activity$interval <- as.numeric(impute.activity$interval)
colSums(is.na(impute.activity))

summary(impute.activity)

# Summarize data by date
impute.activity2 <- summarize(group_by(impute.activity,date),daily.step=sum(steps))

mean.impute   <- as.integer(mean(impute.activity2$daily.step))
mean.impute

median.impute <- as.integer(median(impute.activity2$daily.step))
median.impute

# Plot histogram
plot.steps.day <- ggplot(impute.activity2, aes(x=daily.step)) + 
  geom_histogram(binwidth = 1000, aes(y=..count.., fill=..count..)) + 
  geom_vline(xintercept=mean.impute, colour="red", linetype="dashed", size=1) +
  geom_vline(xintercept=median.impute, colour="green" , linetype="dotted", size=1) +
  labs(title="Histogram of Number of Steps taken each day (impute)", y="Frequency", x="Daily Steps")
plot.steps.day

# Qns 5: Are there differences in activity patterns between weekdays and weekends?

impute.activity$day <- ifelse(weekdays(impute.activity$date) %in% c("Saturday","Sunday"), "weekday", "weekend")

# Preparing data for ggplot
impute.df <- impute.activity %>% group_by(interval,day) %>% summarise(mean.step=mean(steps))

# Plot Average steps across weekday/weekend vs 5-min interval Time Series
plot.weekday.interval <- ggplot(impute.df, aes(x=interval, y=mean.step, color=day)) + 
  facet_grid(day~.) +
  geom_line() + 
  labs(title="Average Number of Steps Taken vs 5-min Interval on Weekday/Weekend", y="Average Number of Steps", x="5-min Interval Times Series")
plot.weekday.interval

