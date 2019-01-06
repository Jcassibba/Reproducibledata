# Reproducibledata
---
title: "PA1_template.Rmd"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r}
library(ggplot2)
#loading the data

activity <- read.csv("activity.csv",TRUE,",")
activity$date <- as.POSIXct(activity$date, "%Y-%M-%D")
weekday <- weekdays(activity$date)
activity <- cbind(activity,weekday)
```
#average number of steps per day
```{r}
activity_total_steps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(activity_total_steps) <- c("date", "steps")
hist(activity_total_steps$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "darkblue", ylim = c(0,20), breaks = seq(0,25000, by=2500))
```
# the average total steps
```{r}
mean(activity_total_steps$steps)
```
# the median of the total steps
```{r}
median(activity_total_steps$steps)
```
#average daily activity pattern
```{r}
average_daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(average_daily_activity) <- c("interval", "mean")
plot(average_daily_activity$interval, average_daily_activity$mean, type = "l", col="darkblue", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")
```
# average daily activity per every 5 mins
```{r}
average_daily_activity[which.max(average_daily_activity$mean), ]$interval
```
# determining and correcting for missing values 
```{r}
sum(is.na(activity$steps))
imputed_steps <- average_daily_activity$mean[match(activity$interval, average_daily_activity$interval)]
activity_imputed <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed_steps, no = activity$steps))
total_steps_imputed <- aggregate(steps ~ date, activity_imputed, sum)
names(total_steps_imputed) <- c("date", "daily_steps")
```
#plotting total number of steps with missing data filled in
```{r}
hist(total_steps_imputed$daily_steps, col = "darkblue", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))
# mean and median of total number of steps with missing data filled in
mean(total_steps_imputed$daily_steps)
median(total_steps_imputed$daily_steps)
```
#determining difference between weekday and weekend activity
```{r}
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
        if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })
```
# plotting difference between weekday and weekend activity
```{r}
activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
       geom_line() +
       labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
```
