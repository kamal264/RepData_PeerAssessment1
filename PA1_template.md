---
output: html_document
---
**Rep_Data Peer Assesment 1**
==========================
*Kamal*


Here is the Code to Load the data



```r
activity <- read.table("./data/activity.csv",sep=",",header = T)
### We can ignore the missing values
c_activity <- activity[complete.cases(activity),]
```

What is mean total number of steps taken per day?

```r
##aggregate of daily steps
d_steps <- aggregate(steps~date,data=c_activity, sum)

##Create a histogram of the total number of steps taken each day
hist(d_steps$steps, col = "blue", main = "Steps per Day",xlab ="Daily Steps")
```

![plot of chunk meanplot](figure/meanplot.png) 

```r
## Mean and Maidan
meansteps<-as.integer(mean(d_steps$steps))
mediansteps<-median(d_steps$steps)
```
The mean total of steps per day is 10766. The median total of steps per day is 10765.


What is the average daily activity pattern?

```r
#i_steps <- aggregate(steps~interval,data=c_activity, mean)
i_steps <- aggregate(c_activity$steps,list(interval = c_activity$interval),mean)

plot (i_steps$interval,
      i_steps$x,
      ylab ="Steps", 
      xlab ="5 Min Intervals", 
      type="l")
```

![plot of chunk interval](figure/interval.png) 
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max<-i_steps[
  (i_steps$steps==max(i_steps$steps))  ## row that has the max value
  ,]  ## Return the Interval
```

```
## Warning: no non-missing arguments to max; returning -Inf
```
Maximum number of steps are  and date is .


Imputing missing values

```r
NAs <- sum(is.na(activity$steps))
NAs
```

```
## [1] 2304
```

```r
#data without NA
## Merging with Interval data to get mean of 5 intervals
nonNA_activity <- merge(activity,i_steps)

##Replacing the NAs with corresponding 5minutes interval Steps Mean value 
nonNA_activity$steps <- ifelse(is.na(nonNA_activity$steps),nonNA_activity$x
                               ,nonNA_activity$steps)


fd_steps <- aggregate(steps~date,data=nonNA_activity, sum)
hist(fd_steps$steps, col = "blue", main = "Steps per Day with No NA ",xlab ="Daily Steps")
```

![plot of chunk misisng](figure/misisng.png) 

```r
#Calculate the Mean and Median of the new datatset.

meansteps<-as.integer(mean(fd_steps$steps))
mediansteps<-as.integer(median(fd_steps$steps))
```
The mean total of steps per day is 10766. The median total of steps per day is 10766. 


Are there differences in activity patterns between weekdays and weekends?
Compare weekend vs weekday data. Are there any differences?



```r
nonNA_activity$day <- ifelse( weekdays(as.Date(nonNA_activity$date)) %in% c("Saturday","Sunday"),"weekend","weekday")

weekend_df <- nonNA_activity[
  (nonNA_activity$day == "weekend"),
  ]

weekday_df <- nonNA_activity[
  (nonNA_activity$day == "weekday"),
  ]

i_steps_we <- aggregate(weekend_df$steps,list(interval = weekend_df$interval), mean)
i_steps_wd <- aggregate(weekday_df$steps,list(interval = weekday_df$interval), mean)
#Now plot weekend and weekday activity patterns.

par(mfcol = c(2,1))

plot (i_steps_we$interval,
      i_steps_we$x,
      ylab ="Steps", 
      xlab ="5 Min Intervals",
      main = "Weekend Average Steps",
      type="l")

plot (i_steps_wd$interval,
      i_steps_wd$x,
      ylab ="Steps", 
      xlab ="5 Min Intervals",
      main = "Weekday Average Steps",
      type="l")
```

![plot of chunk weekdays](figure/weekdays.png) 
