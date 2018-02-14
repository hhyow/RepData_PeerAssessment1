getwd()
setwd('C:/Coursera/Data/module-5/w2/RepData_PeerAssessment1')
getwd()
# Read data set
if(!file.exists("../original_data_set")){dir.create("../original_data_set")}
data_set_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(data_set_url,
              destfile="../original_data_set/activity.zip")

# Unzip dataSet
unzip(zipfile="../original_data_set/activity.zip",
      exdir="../original_data_set")

#------------------------------------------------------------------------------
# Loading and preprocessing the data
#------------------------------------------------------------------------------
#   Show any code that is needed to

#   1. Load the data (i.e. read.csv())
   
activity_table <- read.table("../original_data_set/activity.csv", 
                        header=TRUE, quote="\"", sep=",", na.strings = "NA", 
                        colClasses = c('numeric','character','numeric'))

#   2. Process/transform the data (if necessary) into a format suitable for 
#   your analysis

# Convert the Date variable to Type Date
activity_table$date <- as.Date(activity_table$date, "%Y-%m-%d")
# Remove incomplete observation
activity_table_no_NA <- activity_table[complete.cases(activity_table),]

#------------------------------------------------------------------------------
# What is mean total number of steps taken per day?
#------------------------------------------------------------------------------
#   For this part of the assignment, you can ignore the missing values in the 
#   dataset.
#

#   1. Make a histogram of the total number of steps taken each day

steps_taken_each_day <- aggregate(steps ~ date, activity_table_no_NA, sum)

hist(steps_taken_each_day$steps, 
     main="Histogram of the total number of steps taken each day", 
     xlab = "Number of steps taken each day", 
     col="red")

#   2. Calculate and report the mean and median total number of steps taken 
#   per day

steps_taken_each_day_mean <- mean(steps_taken_each_day$steps)
print(steps_taken_each_day_mean)

steps_taken_each_day_median <- median(steps_taken_each_day$steps)
print(steps_taken_each_day_median)

#------------------------------------------------------------------------------
# What is the average daily activity pattern?
#------------------------------------------------------------------------------
#   1. Make a time series plot (i.e. type = "l") of the 5-minute interval 
#   (x-axis) and the average number of steps taken, averaged across all days 
#   (y-axis)

average_steps_interval <- 
  aggregate(steps ~ interval, FUN=mean, data=activity_table_no_NA)

plot(x=average_steps_interval$interval
     , y=average_steps_interval$steps
     , type="l"
     , main="Average number of steps taken across all days"
     , xlab="5-minute interval"
     , ylab="Average number of steps taken")

#   2. Which 5-minute interval, on average across all the days in the dataset, 
#   contains the maximum number of steps?

interval_contains_max_steps <- 
  average_steps_interval$interval[which.max(average_steps_interval$steps)]

print(interval_contains_max_steps)

#------------------------------------------------------------------------------
# Imputing missing values
#------------------------------------------------------------------------------
#   Note that there are a number of days/intervals where there are missing 
#   values (coded as NA). The presence of missing days may introduce bias into 
#   some calculations or summaries of the data.

#   1. Calculate and report the total number of missing values in the dataset 
#   (i.e. the total number of rows with NAs)

total_number_of_missing_values <- sum(is.na(activity_table$steps))
print(total_number_of_missing_values)

#   2. Devise a strategy for filling in all of the missing values in the 
#   dataset. The strategy does not need to be sophisticated. For example, 
#   you could use the mean/median for that day, or the mean for that 5-minute 
#   interval, etc.
#   3. Create a new dataset that is equal to the original dataset but with the 
#   missing data filled in.

activity_table_impute_NA <- activity_table

for (i in 1:nrow(activity_table_impute_NA)) {
  if(is.na(activity_table_impute_NA$steps[i])) {
    filling_value <- average_steps_interval$steps[which(average_steps_interval$interval 
                                              == activity_table_impute_NA$interval[i])]
    activity_table_impute_NA$steps[i] <- filling_value 
  }
}

#   4. Make a histogram of the total number of steps taken each day and 
#   Calculate and report the mean and median total number of steps taken per 
#   day. Do these values differ from the estimates from the first part of the 
#   assignment? What is the impact of imputing missing data on the estimates 
#   of the total daily number of steps?
steps_taken_each_day_impute_NA <- aggregate(steps ~ date, 
                                            activity_table_impute_NA, sum)
hist(steps_taken_each_day_impute_NA$steps, 
     main="Histogram of the total number of steps taken each day (NAs are imputed)", 
     xlab = "Number of steps taken each day", 
     col="red")

steps_taken_each_day_mean_impute_NA <- 
  mean(steps_taken_each_day_impute_NA$steps)
print(steps_taken_each_day_mean_impute_NA)

steps_taken_each_day_median_impute_NA <- 
  median(steps_taken_each_day_impute_NA$steps)
print(steps_taken_each_day_mean_impute_NA)

#------------------------------------------------------------------------------
# Are there differences in activity patterns between weekdays and weekends?
#------------------------------------------------------------------------------
#   For this part the weekdays() function may be of some help here. Use the 
#   dataset with the filled-in missing values for this part.

#   1. Create a new factor variable in the dataset with two levels -- "weekday"
#    and "weekend" indicating whether a given date is a weekday or weekend day.
activity_table_impute_NA$weekdays <- 
  as.factor(ifelse(weekdays(activity_table_impute_NA$date) %in% 
                     c("Saturday", "Sunday"), "weekend", "weekday"))
#   2. Make a panel plot containing a time series plot (i.e. type = "l") of the
#    5-minute interval (x-axis) and the average number of steps taken, averaged
#    across all weekday days or weekend days (y-axis). The plot should look 
#    something like the following, which was created using simulated data:

average_steps_interval <- aggregate(steps ~ interval + weekdays, FUN=mean, 
                                    data=activity_table_impute_NA)

library(lattice)

xyplot(steps ~ interval | weekdays, 
       data = average_steps_interval, 
       type = "l", 
       layout = c(1, 2), 
       xlab = "5-minute interval", 
       ylab = "Average number of steps taken")
