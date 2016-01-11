library(ggplot2);library(plyr)
data = read.csv("activity.csv")

#remove NA steps
dataWithoutNA = subset(data, !is.na(steps))

#What is mean total number of steps taken per day?
stepsPerDay = tapply(dataWithoutNA$steps , dataWithoutNA$date, FUN=sum)
stepsPerDay = stepsPerDay[!is.na(stepsPerDay)]
stepsPerDay = as.data.frame(stepsPerDay)
stepsPerDay = cbind(stepsPerDay, rownames(stepsPerDay))
colnames(stepsPerDay) = c('steps', 'date')
binwidth = (range(stepsPerDay$steps)[2] - range(stepsPerDay$steps)[1])/10
ggplot(data=stepsPerDay, aes(steps)) + geom_histogram(binwidth=binwidth) + geom_vline(aes(xintercept=mean(steps)), color="red", linetype="dashed", size=1)
meanStepsPerDay = mean(stepsPerDay$steps)
medianStepsPerDay = median(stepsPerDay$steps)


#What is the average daily activity pattern?
stepsPerInterval = tapply(dataWithoutNA$steps , dataWithoutNA$interval, FUN=mean)
stepsPerInterval = as.data.frame(stepsPerInterval)
stepsPerInterval = cbind(stepsPerInterval, as.numeric(rownames(stepsPerInterval)))
colnames(stepsPerInterval) = c('steps', 'interval')
intervalWithMax=stepsPerInterval[which.max(stepsPerInterval$steps),2]
ggplot(data=stepsPerInterval, aes(x=interval, y=steps)) +  geom_line() + ylab("Number of steps") + geom_vline(aes(xintercept=intervalWithMax), color="red", linetype="dashed", size=1)

#Imputing missing values
sum(is.na(data$steps))
sum(is.na(data$date))
sum(is.na(data$interval))

replaceNa = function(row) {
  if(is.na(row[1])) {
    meanInterval = stepsPerInterval[row[3],1]
    meanInterval = if(is.na(meanInterval)) 0 else meanInterval
    as.numeric(meanInterval)
  } else {
    as.numeric(row[1])
  }
}

dataWithReplacedNA = data
dataWithReplacedNA$steps = apply(data,1,replaceNa)
dataWithReplacedNA$oldSteps = data$steps

stepsPerDayReplacedNA = tapply(dataWithReplacedNA$steps , dataWithReplacedNA$date, FUN=sum)
stepsPerDayReplacedNA = as.data.frame(stepsPerDayReplacedNA)
stepsPerDayReplacedNA = cbind(stepsPerDayReplacedNA, rownames(stepsPerDayReplacedNA))
colnames(stepsPerDayReplacedNA) = c('steps', 'date')
binwidth = (range(stepsPerDayReplacedNA$steps)[2] - range(stepsPerDayReplacedNA$steps)[1])/20
ggplot(data=stepsPerDayReplacedNA, aes(steps)) + geom_histogram(binwidth=binwidth) + geom_vline(aes(xintercept=mean(steps)), color="red", linetype="dashed", size=1)
meanWithReplacedNa = mean(stepsPerDayReplacedNA$steps)
meanWithReplacedNa
meanStepsPerDay

medianWithReplacedNa = median(stepsPerDayReplacedNA$steps)
medianWithReplacedNa
medianStepsPerDay

#Are there differences in activity patterns between weekdays and weekends?
dataWithReplacedNA$weekday = weekdays(as.Date(as.character(dataWithReplacedNA$date))) 
dayType = function(weekday) {
  if(weekday %in% c('Sunday', 'Saturday')) {
    'weekend'
  } else {
    'weekday'
  }
}

dataWithReplacedNA$dayType = apply(dataWithReplacedNA,1, function(row) {dayType(row['weekday'])})

stepsPerIntervalAndDayType = ddply(dataWithReplacedNA, .(interval, dayType), summarize, steps=sum(steps))
ggplot(data=stepsPerIntervalAndDayType, aes(x=interval, y=steps)) + ylab("Number of steps") +  geom_line() + facet_wrap(~ dayType, ncol=1)


