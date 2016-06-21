-   

author: "Kathy0305"

Reproducible Research: Peer Assessment 1
========================================

### The data in this assignment can be downloaded from the course web site:

### [Activity Monitoring Data](https://archive.ics.uci.edu/ml/datasets/PAMAP2+Physical+Activity+Monitoring)

##### Dataset: Activity monitor data

##### The variables included in this dataset are:

-**steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA )
-**date**: The date on which the measurement was taken in YYYY-MM-DD format
-**interval**: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Check to see if data exists in directory if file does not exists create a directory download the file and unzip

``` r
if (!file.exists("./UCI HAR Dataset")) {
    dir.create("Data")
    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url, "Dataset.zip")
    unzip("Dataset.zip") 
}
```

    ## Warning in dir.create("Data"): 'Data' already exists

Read the data and put it in a data frame called 'data'

``` r
data <- read.table("activity.csv", header = TRUE, sep = ",")
```

Keep track of date data got downloaded

``` r
dateDownLoaded <- date()
dateDownLoaded
```

    ## [1] "Tue Jun 21 11:55:24 2016"

Get an idea of what the data looks like

``` r
head(data)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

``` r
summary(data)
```

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

``` r
str(data)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

Process/transform the data (if necessary) into a format suitable for your analysis the data contains only 3 variables. The formats of variable steps and interval are integers. The format of date is Factor. Change class of date to Date

``` r
data$date <- as.Date(as.character(data$date), "%Y-%m-%d")
str(data)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

So we have 3 variables, with 2304 missing values.

#### WHAT IS MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY?

------------------------------------------------------------------------

For this part of the assignment, you can ignore the missing values in the dataset. Calculate the total number of steps taken per day

``` r
TotalSteps <- aggregate(steps ~ date, data = data, FUN = sum, na.rm=TRUE)
```

Lets see what does it look like now

``` r
str(TotalSteps)
```

    ## 'data.frame':    53 obs. of  2 variables:
    ##  $ date : Date, format: "2012-10-02" "2012-10-03" ...
    ##  $ steps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...

Make a Histogram of the total number of steps taken each day Use Hist function in base R

``` r
hist(TotalSteps$steps,  xlab = "Steps", main = "Total Steps per Day", col = "cyan")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-8-1.png)

The basic histogram doesnot convey much
lets add some baselines

lets look at the mean and median of TotalSteps (round to the nearest integer)

``` r
MeanTotalSteps <- round(mean(TotalSteps$steps))
MeanTotalSteps
```

    ## [1] 10766

``` r
MedianTotalSteps <- round(median(TotalSteps$steps))
MedianTotalSteps
```

    ## [1] 10765

It looks that the Mean and Median are similar around 10766 steps

Let's add the Mean and Median as baselines to the plot

``` r
## add baseline to the graph
hist(TotalSteps$steps,  xlab = "Steps", main = "Total Steps per Day", col = "cyan")
abline(v=MeanTotalSteps, col="red", lwd = 6) ## made this line large so that we can see the differenece
abline(v=MedianTotalSteps, col="yellow", lwd = 1)   
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-10-1.png)

#### WHAT IS THE AVERAGE DAILY ACTIVITY PATTERN?

------------------------------------------------------------------------

##### Make a time series plot of the 5-minute interval (x-axis) and the

##### average number of steps taken, averaged across all days (y-axis)

##### Which 5-minute interval, on average across all the days in the dataset,

##### contains themaximum number of steps?

Time series plot of the average number of steps taken
use plot.ts function in base R

``` r
StepInterval <- aggregate(steps ~ interval, data=data,FUN= mean,na.rm = TRUE)
plot.ts(StepInterval$steps, ylab = "Average Number of Steps", xlab = "5-minute interval",
        main = "Average number of steps per 5 mins interval", col="blue")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-11-1.png)

This is just shows us the index x-axis and not the actual interval
looks like its a little above 200 steps and at about 110 index
Need to add some baselines to understand the data better.

##### Which 5-minute interval, on average across all the days in the dataset,contains the maximum number of steps?

``` r
Max <- StepInterval[which.max(StepInterval$steps),]
maxSteps <- round(max(StepInterval$steps))
maxInterval <- as.integer(with(StepInterval, StepInterval[steps == max(steps), "interval"]))
maxSteps
```

    ## [1] 206

``` r
maxInterval
```

    ## [1] 835

looks like the maximum 5-minute interval is 835
maximum steps 206

lets plot that with some baseline
Will need ggplot2 package

``` r
library(ggplot2)                        ## call plotting package ggplots
```

    ## Warning: package 'ggplot2' was built under R version 3.2.5

``` r
vlineData <- data.frame(maxInterval)    ## create new dataframe for easier plotting
ggplot(data=StepInterval, aes(x=interval, y=steps)) +
    geom_line(col="blue") +
    xlab("5-minute interval") +
    ylab("average number of steps taken")+
    ggtitle("Average number of steps per 5 mins interval") +
         geom_vline(xintercept = maxInterval, col= "red", size= 1) +
    geom_text(aes(maxInterval,0, label = " 835", col="red",angle=90, vjust=-0.4, hjust=0))   
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-13-1.png)

#### MISSING VALUES

------------------------------------------------------------------------

##### Code to describe and show a strategy for imputing missing data

Lets take another look at our data and see how many missing values are there and where?

``` r
  summary(data)
```

    ##      steps             date               interval     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
    ##  NA's   :2304

we have 2304 missing values
lets looks where

``` r
sum(is.na(data$steps))
```

    ## [1] 2304

``` r
sum (is.na(data$interval))
```

    ## [1] 0

``` r
sum (is.na(data$date))
```

    ## [1] 0

it looks that only variable/column missing values is 'steps'
There are several ways to handle missing values.
Will use the simple imputation technique.

**Single imputation technique** replaces missing values with the means of the
other values in the variable.**Mean imputation** preserves the mean of the variable.

To do mean imputation we simply need to identify missing values,
calculate the mean of the remaining values, and store that mean into those missing value positions:

``` r
NewData <- data ## copy the original data frame so that I can replace the missing values

## find the mean of the remaining values in 'steps' and put them where ever you see NA
NewData[is.na(NewData)] <- mean(NewData$steps, na.rm = TRUE)
```

Lets plot the new data that contains no missing values

``` r
NewTotalSteps <- aggregate(steps ~ date, data = NewData, FUN = sum, na.rm=TRUE)

hist(NewTotalSteps$steps,  xlab = "Steps", main = "Total Steps per Day", col = "cyan")
abline(v=mean(NewTotalSteps$steps), col="red", lwd = 4)
text(mean(NewTotalSteps$steps),35,labels="mean", pos=4, col="red")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-17-1.png)

lets plot the two data frames next to each other to see if there is any difference

``` r
par(mfrow=c(1,2))
hist(TotalSteps$steps, ylim=c(0,35) ,  xlab = "Steps", main = "Total Steps per Day", col = "cyan")
abline(v=MeanTotalSteps, col="red", lwd = 4) 
text(mean(TotalSteps$steps),25,labels="mean", pos=4, col="red")
abline(v=MedianTotalSteps, col="yellow", lty=3)
text(median(TotalSteps$steps), 22, col="yellow",pos = 4, labels = "median")

hist(NewTotalSteps$steps,  xlab = "Steps", main = "Total Steps per Day,NA replaced", col = "cyan")
abline(v=mean(NewTotalSteps$steps), col="red", lwd = 4)
text(mean(NewTotalSteps$steps),35,labels="mean", pos=4, col="red")
abline(v=median(NewTotalSteps$steps),col="yellow",lty=3)
text(median(NewTotalSteps$steps),32,col = "yellow",  pos=4,labels="median")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-18-1.png)

What is the impact of imputing missing data on the estimates of the total daily number of steps?
As we can see from the plots, visually the mean and medians for both the data files are the same

*the single imputation technique did not change the integrety of the data*
the only impact was the increase of observations( frequency)

#### ARE THERE DIFFERENCES IN ACTIVITY PATTERNS BETWEEN WEEKDAYS AND WEEKENDS?

------------------------------------------------------------------------

##### Panel plot comparing the average number of steps taken per 5-minute interval across

##### weekdays and weekends

Lets take a look at our data again

``` r
   head(NewData)
```

    ##     steps       date interval
    ## 1 37.3826 2012-10-01        0
    ## 2 37.3826 2012-10-01        5
    ## 3 37.3826 2012-10-01       10
    ## 4 37.3826 2012-10-01       15
    ## 5 37.3826 2012-10-01       20
    ## 6 37.3826 2012-10-01       25

-The date variable is in the YYYY-MM-DD format
-no indication if day is weekend (Saturday and Sunday)or weekday

we need to find the day name of each date and then group them by weekend and weekday

Will use package 'chron' handles dates in R

``` r
library(chron)
```

    ## Warning: package 'chron' was built under R version 3.2.4

``` r
## decided to use packege 'chron' to sort out the weekdays vs weekend
## get a boolean T/F answer
## add a column called 'weekend' to the NewData dataframe 
NewData$weekend = chron::is.weekend(NewData$date) ## is it a weekend?? 

## add up the steps by weekend or weekdays and get the mean
stepsByDay <- aggregate(NewData$steps ~ NewData$interval + NewData$weekend, NewData, mean)
##Use the dataset with the filled-in missing values for this part.
```

Rename the variables to clearer ones

``` r
names(stepsByDay) <- c("interval", "day", "steps") ## change names of columns for plot
```

Lets take a look at the plots separated by weekend vs weekday

``` r
day_names <- c(`TRUE` = "WeekEnd", `FALSE` = "WeekDay") ## change the legend names
ggplot(stepsByDay, aes(interval, steps, color=day)) + geom_line() + theme(legend.position = "none")+ facet_grid(day ~ .,labeller = as_labeller(day_names)) + xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-22-1.png)

##### **Its looks that although there is a higher peak during the weekdays, there is more uniform activity throught the weekend.**
