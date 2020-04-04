Activity
================

# Tracking the activity of an individual for each day for the months of October and November at 5 minute intervals.

Load Activity.csv here:

``` r
Activity<- read.csv("Activity.csv")
Activity$Date<-Activity$date
Activity<-Activity[,c(4,3,1)]
```

Here is a histogram followed by a summary of the total steps taken per
day

``` r
steps_per_Day<-tapply(Activity$steps,Activity$Date,sum)
hist(steps_per_Day)
```

![](Activity_files/figure-gfm/histogram1%20steps%20per%20Day-1.png)<!-- -->

``` r
xt<-summary(steps_per_Day)
print(xt, type="html")
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##      41    8841   10765   10766   13294   21194       8

``` r
mean<-summary(steps_per_Day)["Mean"]
median<-summary(steps_per_Day)["Median"]
```

We can report that the test subject took on average 1.076618910^{4}
steps per day. The median value for the number of steps per day is
1.076510^{4}.

Create a time series plot of the average number of steps taken. First,
remove all the raws with NA values, then find the average steps taken
for each interval 0 through 235. The maximum average step values is at
interval 835.

``` r
Activity_without_NA<-subset(Activity,!is.na(Activity$steps))
Avg_steps_by_interval<-tapply(Activity_without_NA$steps,(Activity_without_NA$interval),mean)
with(Activity_without_NA,plot(unique(interval),Avg_steps_by_interval,type="l"))
abline(v=835,col="red")
```

![](Activity_files/figure-gfm/remove%20NA-1.png)<!-- -->

Here, replace each NA steps value for each time interval by its
corresponding average step value for that interval. First, get a data
frame of all steps value = NA

``` r
Activity_NA<-subset(Activity,is.na(Activity$steps))
number_Na_values<-nrow(Activity_NA)
```

There are `number_Na_values` NA values in the Activity data frame

Second, replace steps column in the **Activity\_NA** data frame by 8
times the Avg\_steps\_by\_interval vector since there are a days in the
data frame. Raw bind the two data frames **Activity\_NA** and
**Activity\_without\_NA**, then use the *arrange* function of the
**lubridate** library to order the combined data frame in ascending
order of date.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
Activity_NA$steps<-rep(Avg_steps_by_interval,8)

Activity_new<-rbind(Activity_without_NA,Activity_NA)
Activity_new<-arrange(Activity_new,Activity_new$Date)
```

Here is a histogram followed by a summary of the total steps taken per
day with the new Activity dataf frame where all missing values have been
imputed.

``` r
new_steps_per_Day<-tapply(Activity_new$steps,Activity_new$Date,sum)
hist(new_steps_per_Day)
```

![](Activity_files/figure-gfm/histogram2%20steps%20per%20Day-1.png)<!-- -->

``` r
xt<-summary(new_steps_per_Day)
print(xt, type="html")
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    9819   10766   10766   12811   21194

``` r
mean_val<-summary(new_steps_per_Day)["Mean"]
median_val<-summary(new_steps_per_Day)["Median"]
```

The mean and median steps per day are 1.076618910^{4} and
1.076618910^{4}. Those values are similar to the mean and median values
obtained earlier.

plot comparing the average number of steps taken per 5-minute interval
across weekdays and
weekends

``` r
Activity_new["Weekday"]<-weekdays(as.Date(as.character(Activity_new$Date)))
Activity_new$Weekday<-as.factor(Activity_new$Weekday)

Activity_new_weekend<-subset(Activity_new,Weekday %in% c("Saturday","Sunday"))
Activity_new_weekday<-subset(Activity_new,Weekday %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))

Avg_steps_by_interval_weekday<-tapply(Activity_new_weekday$steps,(Activity_new_weekday$interval),mean)
Avg_steps_by_interval_weekend<-tapply(Activity_new_weekend$steps,(Activity_new_weekend$interval),mean)


par(mfrow=c(2,1), mar=c(4,4,2,1))
with(Activity_new_weekend,plot(unique(interval),Avg_steps_by_interval_weekend,type="l",main="weekend", xlab ="" ,ylab="Number 0f steps"))
with(Activity_new_weekday,plot(unique(interval),Avg_steps_by_interval_weekday,type="l",main="weekday", xlab ="Interval" ,ylab="Number of steps")) 
```

![](Activity_files/figure-gfm/line%20plot-1.png)<!-- -->
