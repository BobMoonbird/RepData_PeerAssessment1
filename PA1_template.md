## Loading and preprocessing the data

So, to start with, we load the data and turn dates and intervals into as.date and as.factor:

```r
activity = read.csv("activity.csv") #read the file
activity$interval = as.factor(activity$interval) #intervals should be factors for futhers tasks
activity$date = as.Date(activity$date) #dates should be seen as dates for further tasks#
```


## What is mean total number of steps taken per day?
Next find out total number of steps for ech day, the mean and the median for all days.  
I was supposed to make a histogram for total number of steps, but I also made a plot for number of steps for each day.

```r
dates = unique(activity$date)
sums = vector()
for(i in 1:length(dates))
{sums[i]=sum(activity$steps[which(activity$date==dates[i] &!is.na(activity$steps))])}
sbd = data.frame(dates, sums) #total nmber of steps taken per day


hist(sbd$sums, breaks = 100, main = "Total number of steps for all days", xlab = "Number of steps per day") #histogram for the total number of steps taken each day
```

<img src="figure/unnamed-chunk-2-1.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" style="display: block; margin: auto;" />

```r
plot(sbd$dates, sbd$sums, type = "p", xlab = "Dates", ylab = "Number of steps on this day", main = "Number of steps for each day") #showing how total number of steps distributes across all dates
```

<img src="figure/unnamed-chunk-2-2.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" style="display: block; margin: auto;" />

```r
mean(sbd$sums) #mean for the total number of steps taken each day
```

[1] 9354.23

```r
median(sbd$sums) #median for the total number of steps taken each day
```

[1] 10395

So here you got it:  
the median is 10395 step,  
the mean is 9354.23

## What is the average daily activity pattern?
Talking about the daily patterns, I needed to do some job on avareging the thing across each interval.  
Then I decided to create a data frame with names of intervals and its avareges.  
There you can see the plot for that avarege and the calculus for max avarege interval.  

```r
intervals = unique(activity$interval)
intmeans = vector()
for(i in 1:length(intervals))
{intmeans[i]=mean(activity$steps[which(activity$interval==intervals[i] &!is.na(activity$steps))])}
meanbi = data.frame(intervals, intmeans)
plot(meanbi, type = "l", col = "green", main = "Avarege steps on each interval acoss all days", xlab = "Intervals", ylab = "Number of steps") #plot for the mean steps for each interval across all days
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
meanbi[which(meanbi$intmeans==max(meanbi$intmeans)),1] #5-minute interval that on average contains most number of steps 
```

```
## [1] 835
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```

## Imputing missing values
### 3.1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
Going point by point: the total number of missing values: 2034

```r
length(which(is.na(activity$steps)==TRUE))
```

```
## [1] 2304
```

### 3.2 NAs missing strategy (takign the decision)
Then need to tae a look at how those NAs are distributed.  
I found out that there are several totally missed days, so the best alternative is to take the avarege for each interval.  

```r
nas = activity[which(is.na(activity$steps)==TRUE),]
table(nas$date)
```

```
## 
## 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10 
##        288        288        288        288        288        288 
## 2012-11-14 2012-11-30 
##        288        288
```

### 3.3 filling NAs with mean for the interval, as filling it with mean fort he day is useless
Now I fill the missing values with that avarege, creating new data set and new variable.  


```r
activity_na = activity
activity_na[,4] = rep(meanbi$intmeans, 61)

for(i in 1:length(activity_na$steps) )
{
  if (i %in% which(is.na(activity_na$steps))== TRUE) 
  {activity_na[i,5] <- activity_na[i,4]} 
  else
  {activity_na[i,5] <- activity_na[i,1]}
}
names(activity_na)[4] = "mean.for.the.interval"
names(activity_na)[5] = "value.after.NA.strategy"
```

### 3.4 hist, mean, median with NAs filled in
Calculating all same thing for the new data set, with missing values being filled in  

```r
dates = unique(activity$date)
nasums = vector()
for(i in 1:length(dates))
{nasums[i]=sum(activity_na$value.after.NA.strategy[which(activity$date==dates[i])])}
nasbd = data.frame(dates, nasums)
hist(nasbd$nasums, breaks = 100, main = "Total number of steps for all days", xlab = "Number of steps per day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
mean(nasbd$nasums)
```

```
## [1] 10766.19
```

```r
median(nasbd$nasums)
```

```
## [1] 10766.19
```
New median:10766.19  
New mean: 10766.19  
Somehow they are same, I do't know how.  
I even checked with this:  

```r
summary(nasbd$nasums)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

I also decided to show how exactly filled NAs changed the picture:  

```r
plot(activity_na$date, activity_na$value.after.NA.strategy, col = "red", type = "l", main = "Total number of steps for each day with filled missing values", xlab = "Dates", ylab = "Number of steps on this date")
lines(activity$date, activity$steps, col = "green")
temp <- legend("topleft", legend = c("With missing values", "Filled in missing values"), text.width = strwidth("2,000,000"),lty = 1, col = c("green", "red"), xjust = 1, yjust = 1, title = " ")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)
The red lines here are the days with missed values, filled with mean interval each.   
Could have filled with just avarege date figures, but then it would not be possible to see deeper than just a day.  

## Are there differences in activity patterns between weekdays and weekends?
### 4.1 defining weekdays and weekends
Taking the new set (activity_na) and filling in the "weekend/weekday" data.

```r
activity_na[,6] = weekdays(as.Date(activity$date))
days = unique(activity_na$V6)
for(i in 1:length(activity_na$steps))
{
  if(activity_na$V6[i] %in% days[1:5])
  {activity_na$V6[i] = "weekday"}
  #if(activity_na$V6[i] %in% days[6:7])
  else  {activity_na$V6[i]="weekend"}
}
names(activity_na)[6]="type.of.day"
```

### 4.2 plotting weekends and weekdays steps
Now create two new data frames, one for weekend one for weekday avarege for each interval.

```r
weekdaymeans = vector()
for(i in 1:length(intervals))
{weekdaymeans[i]=mean(activity_na$value.after.NA.strategy[which(activity_na$type.of.day=="weekday"&activity_na$interval==intervals[i])])}
daymeanbi = data.frame(intervals, weekdaymeans)

weekendmeans = vector()
for(i in 1:length(intervals))
{weekendmeans[i]=mean(activity_na$value.after.NA.strategy[which(activity_na$type.of.day=="weekend"&activity_na$interval==intervals[i])])}
endmeanbi = data.frame(intervals, weekendmeans)

par(mfrow=c(1,2))
plot(intervals, endmeanbi$weekendmeans, col = "red", type = "l", xlab = "Intervals", ylab = "Mean number of steps on weekends")

plot(intervals, daymeanbi$weekdaymeans, col = "green", type = "l", xlab = "Intervals", ylab = "Mean number of steps on weekdays")
temp <- legend("topright", legend = c("Weekday ", "Weekend"), text.width = strwidth("1,000,000"),lty = 1, col = c("green", "red"), xjust = 1, yjust = 1, title = "Line Types")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

It can be seen from the graph, that weekend activity (the red line) starts later and ends later than weekday (the green line), and is more intense in the day time.  
Overall, the sums of steps for the avarege weekend and weekday are here:  

```r
sum(endmeanbi$weekendmeans)
```

```
## [1] 12201.52
```

```r
sum(daymeanbi$weekdaymeans)
```

```
## [1] 10255.85
```
Weekend: 12201.85  
Weekday: 10255.85  

So yes, we can say that on avarege fitnes tracker users are kore active on weekend then on weekdays.
