setwd("D:/Data Scientist/Reproducible Reasearch")
library(ggplot2) 

activity <- read.csv("D:/Data Scientist/Reproducible Reasearch/activity.csv")

mean_of_steps<- mean(activity$steps, na.rm=TRUE)



steps_by_date <- aggregate(steps ~ date, activity, sum)


names(steps_by_date) <- c("Date", "Total_Steps")

png("plot1.png", width=480, height=480)

ggplot(steps_by_date, aes(x = Total_Steps)) + 
  geom_histogram(fill = "red", binwidth = 1000) + 
  labs(title="Total Steps Per Day", 
       x = "Total Steps", y = "Date") + theme_bw() 

dev.off()


steps_mean   <- mean(steps_by_date$Total_Steps, na.rm=TRUE)
steps_median <- median(steps_by_date$Total_Steps, na.rm=TRUE)



steps_per_interval <- aggregate(activity$steps, 
                                by = list(interval = activity$interval),
                                FUN=mean, na.rm=TRUE)
#convert to integers
##this helps in plotting
##steps_per_interval$interval <- 
  ##as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])
colnames(steps_per_interval) <- c("interval", "steps")

ggplot(steps_per_interval, aes(x=interval, y=steps)) +   
  geom_line(color="orange", size=1) +  
  labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
  theme_bw()

max_interval <- steps_per_interval[which.max(  
  steps_per_interval$steps),]





missing_vals <- sum(is.na(activity$steps))

na_fill <- function(data, pervalue) {
  na_index <- which(is.na(data$steps))
  na_replace <- unlist(lapply(na_index, FUN=function(idx){
    interval = data[idx,]$interval
    pervalue[pervalue$interval == interval,]$steps
  }))
  fill_steps <- data$steps
  fill_steps[na_index] <- na_replace
  fill_steps
}

rdata_fill <- data.frame(  
  steps = na_fill(activity, steps_per_interval),  
  date = activity$date,  
  interval = activity$interval)

##sum(is.na(rdata_fill$steps))

fill_steps_per_day <- aggregate(steps ~ date, rdata_fill, sum)
colnames(fill_steps_per_day) <- c("date","steps")

##plotting the histogram
ggplot(fill_steps_per_day, aes(x = steps)) + 
  geom_histogram(fill = "blue", binwidth = 1000) + 
  labs(title="Histogram of Steps Taken per Day", 
       x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 


steps_mean_fill   <- mean(fill_steps_per_day$steps, na.rm=TRUE)
steps_median_fill <- median(fill_steps_per_day$steps, na.rm=TRUE)



##Are there differences in activity patterns between weekdays and weekends?

weekdays_steps <- function(data) {
  weekdays_steps <- aggregate(data$steps, by=list(interval = data$interval),
                              FUN=mean, na.rm=T)
  # convert to integers for plotting
  weekdays_steps$interval <- 
    as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
  colnames(weekdays_steps) <- c("interval", "steps")
  weekdays_steps
}

data_by_weekdays <- function(data) {
  data$weekday <- 
    as.factor(weekdays(data$date)) # weekdays
  weekend_data <- subset(data, weekday %in% c("Saturday","Sunday"))
  weekday_data <- subset(data, !weekday %in% c("Saturday","Sunday"))
  
  weekend_steps <- weekdays_steps(weekend_data)
  weekday_steps <- weekdays_steps(weekday_data)
  
  weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
  weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))
  
  data_by_weekdays <- rbind(weekend_steps, weekday_steps)
  data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
  data_by_weekdays
}

data_weekdays <- data_by_weekdays(rdata_fill)


ggplot(data_weekdays, aes(x=interval, y=steps)) + 
  geom_line(color="violet") + 
  facet_wrap(~ dayofweek, nrow=2, ncol=1) +
  labs(x="Interval", y="Number of steps") +
  theme_bw()