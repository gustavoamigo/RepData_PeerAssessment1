library(ggplot2)
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


#What is the average daily activity pattern?
stepsPerInterval = tapply(dataWithoutNA$steps , dataWithoutNA$interval, FUN=mean)
stepsPerInterval = as.data.frame(stepsPerInterval)
stepsPerInterval = cbind(stepsPerInterval, as.numeric(rownames(stepsPerInterval)))
colnames(stepsPerInterval) = c('steps', 'interval')
intervalWithMax=stepsPerInterval[which.max(stepsPerInterval$steps),2]
ggplot(data=stepsPerInterval, aes(x=interval, y=steps)) +  geom_line() + geom_vline(aes(xintercept=intervalWithMax), color="red", linetype="dashed", size=1)

#Imputing missing values
rowWithNA = O=is.na(data$steps) is.na(data$date) ||  is.na(data$interval)

