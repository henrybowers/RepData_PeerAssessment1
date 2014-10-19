####
#### Reproducible Research - Peer Assessment 1  ####
####

setwd('~/GitHub/RepData_PeerAssessment1')

## Loading and preprocessing the data

activity <- read.csv('activity.csv',stringsAsFactors=FALSE)
library(lubridate)
activity$date <- ymd(activity$date)

## What is mean total number of steps taken per day?

library(plyr)
stepsPerDay <- ddply(activity, .(month=month(date),day=day(date)), summarize,
                     dailySteps = sum(steps,na.rm = TRUE)
)


hist(stepsPerDay$dailySteps, col = 'blue',xlab = 'Steps Per Day',ylab = 'Number of Days', main = 'Histogram of Daily Steps')
rug(stepsPerDay$dailySteps)

meanStepsPerDay <- mean(stepsPerDay$dailySteps)
medianStepsPerDay <- median(stepsPerDay$dailySteps)

## What is the average daily activity pattern?

meanStepsPerInterval <- ddply(activity, .(interval), summarize, meanSteps = mean(steps, na.rm=TRUE))

maxMeanIntervalSteps <- max(meanStepsPerInterval$meanSteps)
maxIntervalMeanSteps <- meanStepsPerInterval$interval[meanStepsPerInterval$meanSteps==maxMeanIntervalSteps]

plot(meanStepsPerInterval$interval,meanStepsPerInterval$meanSteps, xlab = 'Interval', ylab = 'Mean Steps', main = 'Average Steps per Time Interval', type = 'l', xaxs = 'i' )
abline(v = maxIntervalMeanSteps, col = 'red', lty = 3)


## Imputing missing values

rowsWithMissingValues <- sum(is.na(activity))

activityImputed <- merge(activity, meanStepsPerInterval, by = 'interval', all.x = TRUE)

activityImputed$steps <- ifelse(is.na(activityImputed$steps),activityImputed$meanSteps,activityImputed$steps)

stepsPerDayImputed <- ddply(activityImputed, .(month=month(date),day=day(date)), summarize,
                     dailySteps = sum(steps)
)

hist(stepsPerDayImputed$dailySteps, col = 'red',xlab = 'Steps Per Day',ylab = 'Number of Days', main = 'Histogram of Daily Steps')
rug(stepsPerDayImputed$dailySteps)

meanStepsPerDayImputed <- mean(stepsPerDayImputed$dailySteps)
medianStepsPerDayImputed <- median(stepsPerDayImputed$dailySteps)


## Are there differences in activity patterns between weekdays and weekends?

activity$dayType <- as.factor(ifelse(wday(activity$date) < 6, 'weekday', 'weekend'))

meanStepsPerIntervalPerType <- ddply(activity, .(dayType,interval), summarize, meanSteps = mean(steps, na.rm=TRUE))

library(ggplot2)

plot <- ggplot(meanStepsPerIntervalPerType, aes(x = interval,y = meanSteps))

# overlaid plot
plot + geom_line(aes(col = dayType)) + labs(title="Activity Pattern, Weekdays vs. Weekends") + xlab('Five-Minute Interval') + ylab('Mean Steps') + theme_bw()

# with facets
plot + geom_line(aes(col = dayType)) + facet_wrap(~dayType,ncol=1) + labs(title=" Pattern, Weekdays vs. Weekends") + xlab('Five-Minute Interval') + ylab('Mean Steps') + theme_bw() + theme(legend.position="none")


# imputed version

activityImputed$dayType <- as.factor(ifelse(wday(activityImputed$date) < 6, 'weekday', 'weekend'))

meanStepsPerIntervalPerTypeImputed <- ddply(activityImputed, .(dayType,interval), summarize, meanSteps = mean(steps, na.rm=TRUE))

plot <- ggplot(meanStepsPerIntervalPerTypeImputed, aes(x = interval,y = meanSteps))

# overlaid plot
plot + geom_line(aes(col = dayType)) + labs(title="Activity Pattern, Weekdays vs. Weekends") + xlab('Five-Minute Interval') + ylab('Mean Steps (Imputed)') + theme_bw()

# with facets
plot + geom_line(aes(col = dayType)) + facet_wrap(~dayType,ncol=1) + labs(title=" Pattern, Weekdays vs. Weekends") + xlab('Five-Minute Interval') + ylab('Mean Steps (Imputed)') + theme_bw() + theme(legend.position="none")

# delta

delta <- meanStepsPerIntervalPerTypeImputed$meanSteps - meanStepsPerIntervalPerType$meanSteps
difference <- cbind(meanStepsPerIntervalPerType,delta)

plot <- ggplot(difference, aes(x = interval,y = delta))

# overlaid plot
plot + geom_line(aes(col = dayType)) + labs(title="Delta, Weekdays vs. Weekends") + xlab('Five-Minute Interval') + ylab('Delta Original vs Imputed') + theme_bw()

# with facets
plot + geom_line(aes(col = dayType)) + facet_wrap(~dayType,ncol=1) + labs(title="Delta, Weekdays vs. Weekends") + xlab('Five-Minute Interval') + ylab('Delta') + theme_bw() + theme(legend.position="none")
