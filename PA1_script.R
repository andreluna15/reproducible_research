library(dplyr)
library(ggplot2)

#to set outputs in english
Sys.setlocale("LC_ALL", "en_US")

#reading in temp file
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp)


data <- read.csv(unz(temp, "activity.csv"), stringsAsFactors = F)
data$date <- as.Date(data$date)

daily_steps <- data %>% group_by(date) %>% summarise(total_daily_steps = sum(steps))



#What is mean total number of steps taken per day?

#histogram
ggplot(daily_steps, aes(x = total_daily_steps)) +
  geom_histogram()

#mean and median of steps each day
mean(daily_steps$total_daily_steps, na.rm = T)
median(daily_steps$total_daily_steps, na.rm = T)




#What is the average daily activity pattern?
#mean steps by interval + time series
mean_per_interval <- data %>% group_by(interval) %>% summarise(mean = mean(steps, na.rm = T))

ggplot(mean_per_interval, aes(x = interval, y = mean)) +
  geom_line()

#interval wth mean highest number of steps
mean_per_interval %>% filter(mean == max(mean))


###Imputing missing values

#total number os NAs
summary(data)


#filling missing values

no_missing <- transform(data, steps = ifelse(is.na(steps), ))

data2 <- data
x <- data.frame(0,0,0,0)

for(i in 1:nrow(data2)) {
  if(is.na(data2[i,1] == T)) {
    x <- left_join(data2[i,], mean_per_interval, by = 'interval')
    data2[i,1] <- x[,4]
  }
}


#histogram of data2 - with no missing

daily_steps2 <- data2 %>% group_by(date) %>% summarise(total_daily_steps = sum(steps))

#histogram
ggplot(daily_steps2, aes(x = total_daily_steps)) +
  geom_histogram()

#mean and median of steps each day
mean(daily_steps2$total_daily_steps, na.rm = T)
median(daily_steps2$total_daily_steps, na.rm = T)

#we can see that the mean and median values have no changed for
#the overall data set, but now we have a more complete dataset

mean(data$steps, na.rm = T)
mean(data2$steps)

###Are there differences in activity patterns between weekdays and weekends?

data2 <- data2 %>% mutate(weekday = weekdays(date), day_of_week = ifelse(weekday == "Saturday" | weekday == "Sunday", "weekend", "weekday"))
data2$day_of_week <- as.factor(data2$day_of_week) 

mean_per_interval2 <- data2 %>% group_by(interval, day_of_week) %>% summarise(mean = mean(steps, na.rm = T))

##panel plot
ggplot(mean_per_interval2, aes(x = interval, y = mean)) + 
  geom_line() + 
    facet_grid( day_of_week ~ .)
