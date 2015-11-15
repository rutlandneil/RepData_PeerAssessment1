library(dplyr)
library(lattice)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)
library(chron)
library(knitr)

setwd("C:/Users/The Rutlands/Google Drive/Data Scientist Specialisation/Reproducable Research/RepData_PeerAssessment1")

zipLoc<-'./activity.zip'

unzip(zipLoc)

activity<-read.csv('./activity.csv', colClasses = c('numeric','Date','character'))

activity$date<-as.Date(activity$date)
activity$interval<-as.factor(str_pad(activity$interval,width=4,side='left', pad='0'))

summary(activity)
str(activity)
class(activity$date)

#Calculate and store the number of steps taken each day
daily_steps<-activity %>%
group_by(date) %>%
summarise(steps=sum(steps))

daily_steps

hist(daily_steps$steps, breaks=20)
#Calculate the mean number of steps. Remove NAs or Mean will return NA
mean(daily_steps$steps, na.rm=TRUE)
#Calculate the median number of steps. Remove NAs or Mean will return NA
median(daily_steps$steps, na.rm=TRUE)

#Calculate the average number of steps by interval. Remove NAs or Mean will return NA
avg_steps <- activity %>%
      group_by(interval) %>%
      summarise(average_steps=mean(steps, na.rm=TRUE))

View(avg_steps)

as.numeric(unique(activity$interval))

#plot a time series graph of the 5 minute intervals
plot(x=avg_steps$interval,y=avg_steps$average_steps,type='b', xlab='Time'
     ,ylab='Average No. of Steps', main='Average Steps per Interval')
lines(avg_steps$average_steps)


#Which interval has the highest average number of steps?
top_n(avg_steps,1,average_steps)

#Calculate the number of records where steps aren't recorded
sum(is.na(activity$steps))

plot(x=as.numeric(activity$interval_factor),y=activity$steps)

#subset the rows without steps data
no_steps<-activity[!complete.cases(activity),]

#get average steps from earlier table to fill in blanks
NA_lookup<-left_join(no_steps,avg_steps, by = c('interval' = 'interval'))

#generate data to bind back into original
replaced_NAs<-NA_lookup[,c(4,2,3)]
names(replaced_NAs)<-names(activity)

#remove NAS from original
new_activity<-activity[complete.cases(activity),]
#nrow(new_activity)
#add in rows with calculated averages
new_activity<-rbind(new_activity,replaced_NAs)
#nrow(new_activity)

#Calculate daily steps from new data
new_daily_steps<-new_activity %>%
      group_by(date) %>%
      summarise(steps=sum(steps))

hist(new_daily_steps$steps, breaks=20)
mean(new_daily_steps$steps)
#Calculate the median number of steps. Remove NAs or Mean will return NA
median(new_daily_steps$steps)

write.csv(daily_steps, file='./daily_steps.csv')
write.csv(new_daily_steps, file='./new_daily_steps.csv')


new_activity$weekend<-factor(is.weekend(new_activity$date), levels=c(TRUE,FALSE),
                             labels=c('Weekend','Weekday'))
str(new_activity)

new_avg_steps <- new_activity %>%
group_by(weekend,interval) %>%
summarise(average_steps=mean(steps))

View(new_avg_steps)

xyplot(average_steps ~ interval | weekend, new_avg_steps, layout=c(1,2), type='l')

knit2html('PA1_template.Rmd')