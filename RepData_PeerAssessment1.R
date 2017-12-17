#### 1. Load and preprocess data ####
data <- read.csv(unz("activity.zip", "activity.csv"))

# Preprocess data
summary(data)
ok <- complete.cases(data)
dataWithoutNA <- data[ok, ]
dataWithoutNA$date <- factor(dataWithoutNA$date)

#### 2. What is mean total number of steps taken per day? ####

# Total number of steps taken per day
totalStepsPerDay <- tapply(dataWithoutNA$steps, dataWithoutNA$date, FUN=sum)

# Histogram of the total number of steps taken each day
hist(totalStepsPerDay, breaks = 20,
     main='Histogram of the Total number of steps per day',
     xlab='Total steps per day')

# Mean and median of the total number of steps taken per day
mean(totalStepsPerDay)
median(totalStepsPerDay)

#### 3. What is the average daily activity pattern? ####

# Time series plot of the 5-minute interval (x-axis) and the average number 
# of steps taken, averaged across all days (y-axis)

avgStepsPerInterval <- aggregate( dataWithoutNA$steps ~ dataWithoutNA$interval, FUN = mean )
plot(avgStepsPerInterval[[1]],
     avgStepsPerInterval[[2]],
     type='l', 
     xlab = 'Interval', ylab = 'Average Steps',
     main='Average Steps per Interval over all days')

# Interval with the maximum number of steps on average:
maxSteps <- max(avgStepsPerInterval[[2]])
intervalMaxSteps <- avgStepsPerInterval[avgStepsPerInterval[[2]] == maxSteps,1]
intervalMaxSteps

#### 4. Imputing missing values ####
# Total number of missing values
naCount <- sum(!ok)
naCount

# New dataset equal to the original one, with the missing data filled in.
dataImputed <- data;
dataImputed <- transform(dataImputed, 
                          steps = ifelse(is.na(steps), 
                          ave(steps, interval, FUN = function(x) mean(x, na.rm = TRUE)), 
                          steps))

# Histogram of the total number of steps taken each day
totalStepsPerDayIMPUTED <- tapply(dataImputed$steps, dataImputed$date, FUN=sum)
hist(totalStepsPerDayIMPUTED, breaks = 20,
     main='Histogram of the Total number of steps per day',
     xlab='Total steps per day')

# Mean and median total number of steps taken per day.
mean(totalStepsPerDayIMPUTED)
median(totalStepsPerDayIMPUTED)

#### 5. Activity per weekend v.s. weekdays
dataImputed$day <- ifelse(grepl("S(at|un)", weekdays(as.Date(dataImputed$date), abbr = TRUE)), "weekend", "weekday") 

meanWeekInterval <- aggregate(steps ~ interval + day, dataImputed,  FUN = mean)

library(lattice)
xyplot(steps~interval|factor(day),
       data = meanWeekInterval,
       type='l',layout=c(1,2),
       xlab='Interval',ylab='Number of Steps')

