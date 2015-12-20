# PA1-RR
pdm  
20 July 2015  
In this assignment, we are going to use data from a personal activity monitoring device. We will use some data analysis and generate exploratory plots. The assignment has five sections.

I. Loading & Processing Data
The dataset has three columns. The interval column has intiger value 5 min gap from 0 to 23 h. Hence another column with appropriate timestamp is added. Another additional column specifying weekdays/weekends are added. 



```r
setwd("~/Documents/Data-science/Rerproducible Research/RepData_PeerAssessment1/")
#read data
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
require(scales)
```

```
## Loading required package: scales
```

```r
activity <- read.csv("activity.csv",header = TRUE)

#process data
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
# To makde 4 digit time intervals 
time_num <- formatC(activity$interval, width = "4", flag = "0")
# the colon (:) needs to be introduced to make %H:%M)
time_hmin <- sub("(..)$", ":\\1", time_num)
# merging date(%Y:%m:%d) and time(%H:%M)
date_time2 <- paste( activity$date,time_hmin, sep = " ")
#mutate in dplyr can not be done on strptime or POSIXlt classes
date_time3 <- as.POSIXct(date_time2, "%F %H:%M", tz = "GMT")
date_time <- mutate(activity, date_time3)
```

II. Mean of the total number of steps taken per day


```r
new_activity <- date_time %>%group_by(date) %>% na.omit()
sum_steps <- summarise(new_activity, total_steps = sum(steps))
```

Histogram of the total steps taken each day

```r
hist(sum_steps$total_steps, xlab = "Total steps per day", main = "Histogram of total steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 
The mean and median of the total number of steps taken each day

```r
na1 <- activity %>% na.omit(activity) %>% group_by(date) %>% summarise(total = sum(steps)) %>% summarise(mean = mean(total), median = median(total))
na1
```

```
## Source: local data frame [1 x 2]
## 
##       mean median
## 1 10766.19  10765
```

III. Average daily activity pattern

```r
na2 <- date_time %>% na.omit()
na3 <- na2 %>% group_by(interval) %>% summarise(average_steps = mean(steps))
#A Time column is generated with class POSIXct
time_hmin2 <- as.POSIXct(time_hmin, format = "%H:%M", origin = "2012-10-01", tz = "GMT")
na4 <- mutate(na3, Time = unique(time_hmin2))
#Plot the average number of steps per interval
with(na4, plot(Time, average_steps, type = "l", ylab = "Average Steps", main = "Daily Activity Pattern", xlim = c(min(time_hmin2),max(time_hmin2))))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
#the interval showing the highest average steps.
na5 <- mutate(na3, Time =unique(time_hmin))
subset(na5, average_steps == max(average_steps))
```

```
## Source: local data frame [1 x 3]
## 
##   interval average_steps  Time
## 1      835      206.1698 08:35
```

IV. Imputing missing values


```r
#total number of missing values
sum(is.na(activity))
```

```
## [1] 2304
```

Impute data with Amelia Package

```r
library(Amelia)
```

```
## Loading required package: Rcpp
## ## 
## ## Amelia II: Multiple Imputation
## ## (Version 1.7.3, built: 2014-11-14)
## ## Copyright (C) 2005-2015 James Honaker, Gary King and Matthew Blackwell
## ## Refer to http://gking.harvard.edu/amelia/ for more information
## ##
```

```r
#a.out <- amelia( data, noms = colnames(activity))
#write.amelia(obj=a.out, file.stem = "outdata")
```


Histogram of total number of steps taken each day with the imputed values


```r
#read the amelia outpute file
imp_activity <- read.csv("outdata1.csv", header = TRUE)
new_imp_activity <- imp_activity %>%
        group_by(date) %>%
        summarise(total_steps = sum(mean(steps)))
#plot the data
hist(new_imp_activity$total_steps, xlab = "Total steps per day", main = "Histogram of total steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

Mean and Median of the total number of steps taken in imputed data

```r
Ina <- imp_activity %>% group_by(date) %>% summarise(total = sum(steps)) %>% summarise(mean = mean(total), median = median(total))
Ina
```

```
## Source: local data frame [1 x 2]
## 
##       mean median
## 1 17882.56  11458
```

V. Differences in activity patterns between weekdays and weekends

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day


```r
#add a day column
week <- date_time %>% mutate(day = weekdays(as.Date(as.character(date_time$date), "%Y-%m-%d")))
#convert each workdays as workday
week$day[week$day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")] <- c("Weekday")
#convert each weekends as weekend
week$day[week$day %in% c("Saturday", "Sunday")] <- c("Weekend")
week$day <- as.factor(week$day)
#calculate average steps taken in interval across weekdays/weekends
dfweek <- week %>% group_by(day, interval) %>% na.omit() %>% summarise(Mean_steps = mean(steps))
#add another column for interval as POSIXct
dfweek2 <- mutate(dfweek, Time = unique(time_hmin2))
```
Panel plots showing average number of steps taken

```r
#plot the graph with two facets
#in case I had not set tz parameter, so it was taking local tz and and the date_format will be taking GMT as default. 
# attr(dfweek2$Time, "tzone") <- NULL

ggplot(data= dfweek2, aes(x = Time, y = Mean_steps)) + facet_grid(day ~.) + geom_path(color = "red") + scale_x_datetime(labels = date_format("%H:%M")) + ylab("Average Steps") + ggtitle("Time series of average steps in weekdays/weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
