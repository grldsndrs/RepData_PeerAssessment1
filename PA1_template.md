`## [1] "Mon Jan 11 01:38:38 2016"`

Loading and preprocessing the data
----------------------------------

library('ProjectTemplate') load.project()

*"Project Template"* uses the defualt settings to 'load.project()' the
csv file in the data folder of the working diriectory.

### Data cleaning

    tidyData <- activity[complete.cases(activity),]

### Data tidying

> create a time series by adding the intervals in minutes to the date

dts \<-
as.POSIXct(tidyData*d**a**t**e*) + *m**i**n**u**t**e**s*(*a**s*.*n**u**m**e**r**i**c*(*t**i**d**y**D**a**t**a*interval))

> **choose appropriate classes for features**

    tidyData <- mutate(tidyData,
                       steps= as.numeric(steps),
                       date = as.factor(date),
                       interval=as.numeric(interval))

> **create a time series**

    tidyDataXTS <- xts(tidyData ,order.by = dts,unique = TRUE)

    print(head(sample_n(tidyData,nrow(tidyData))))

    ##       steps       date interval
    ## 12944     0 2012-11-21     2235
    ## 475       0 2012-10-03     1530
    ## 7114      0 2012-10-27     1645
    ## 2345      0 2012-10-11      320
    ## 2077      0 2012-10-10      500
    ## 8349      0 2012-10-31     2340

#### What is mean total number of steps taken per day?

> calculate the ***total number of steps taken per day***

    dys = endpoints(tidyDataXTS, 'days')

    tidyData <- group_by(tidyData, date)%>%
      summarise(meanSteps.Dy = mean(steps),
                medianSteps.Dy=median(steps),
                steps.Dy = sum(steps),
                maxSteps.Dy=max(steps))%>%
      merge(tidyData)%>%
      mutate(meanSteps.Dy.Dys=(meanSteps.Dy*steps.Dy/sum(steps)))

    tidyDataXTS <- xts(tidyData ,order.by = dts,unique = TRUE)

    print(sample(tidyData$steps.Dy,10))

    ##  [1]  7336 12116 10600 12426 10056 10600 13452  4472  6778 15110

> make a histogram of the ***total number of steps taken per day***

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png) \>
calculate and report ***the mean***

    print(sample(tidyData$meanSteps.Dy,10))

    ##  [1] 53.54167 46.15972 11.17708 73.59028 35.35764 24.46875 46.73611
    ##  [8] 44.39931 46.70833 73.59028

> and ***the median***

    print(sample(tidyData$medianSteps.Dy,10))

    ##  [1] 0 0 0 0 0 0 0 0 0 0

> of ***total number of steps taken in a day per day***

#### What is the average daily activity pattern?

> Make a time series plot of the 5-minute interval

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

> ***The interval is indicated by the red dot on the above plot.***

> It is calculated:

    maxNumStepsValue = max(tidyData$steps)
    intervalOfMax = tidyData$interval[tidyData$steps==maxNumStepsValue]
    maxNumStepsDateTime = dts[tidyData$steps==maxNumStepsValue]

    ## [1] 806

    ## [1] 615

    ## [1] "11/27/12 10:15"

Inputing missing values
-----------------------

> > Note that there are a number of days/intervals where there are
> > missing values. The presence of missing days may introduce bias into
> > some calculations or summaries of the data.

> calculate and report the total number of missing values in the dataset

    summary(activity)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

> devise a strategy for filling in all of the missing values in the
> dataset.

> > Use a k nearst neighbors algorithm to predict the missing values
> > based on the known.

    tidyDataSize = nrow(tidyData)
    trainSetIndicies <- sample(x = 1:tidyDataSize,
                               size = floor(.9*tidyDataSize),
                               replace = FALSE)
    trainingSet = tidyData[trainSetIndicies,]%>%
      select(-steps,-medianSteps.Dy)%>%
      mutate(date= as.numeric(date))%>%
      scale()

    testSetIndicies <- setdiff(1:tidyDataSize,trainSetIndicies)
    testSet <- tidyData[testSetIndicies,]%>%
      select(-steps,-medianSteps.Dy)%>%
      mutate(date= as.numeric(date))%>%
      scale()

    observation <- select(tidyData[trainSetIndicies,],steps)%>%
      mutate(steps=as.factor(steps))

    testPredictions <- knn(train = trainingSet,
                           test = testSet,
                           cl = observation$steps,
                           k = floor(1.5*sqrt(tidyDataSize)),
                           prob=TRUE)

> check accuracy of predictions

    sum(as.numeric(tidyData$steps[testSetIndicies]==testPredictions))/length(testPredictions)

    ## [1] 0.7393582

> > 70% good enough for the purposes here.

> can use paramerter setting to predict the missing values

    missingValuesIndicies = is.na(activity$steps)

    > get new raw data

    unTidyData <- activity

    > get new time series

    newDts <- as.POSIXct(unTidyData$date) + minutes(as.numeric(unTidyData$interval ))

    > switch up classes for processing

    unTidyData <- mutate(unTidyData,
                         steps = as.numeric(steps),
                         date = as.factor(date),
                         interval = as.numeric(interval))

    initialize the missing values to the mean of the day if it exists
      or the mean of the data set if not

    initialMissingValues <-
      sapply(activity$date[missingValuesIndicies], function(missingIndexDate) {
        unTidyData$steps[as.numeric(unTidyData$date)==as.numeric(missingIndexDate)] <<-
          # try to find date in complete data
          if(length(tidyData$date[as.numeric(tidyData$date)==as.numeric(missingIndexDate)]))
            # set equal to the first value in the mean per day field
            tidyData$meanSteps.Dy[as.numeric(tidyData$date)==as.numeric(missingIndexDate)][1]
        else
          # set equal to set mean
          mean(tidyData$steps)
      },simplify = TRUE)

    prep data to predict steps with the nearest neighbor classifier

    unTidyData <- group_by(unTidyData,date)%>%
      summarise(meanSteps.Dy = mean(steps),
                medianSteps.Dy = median(steps),
                steps.Dy = sum(steps),
                maxSteps.Dy = max(steps))%>%
      merge(unTidyData)%>%
      mutate(meanSteps.Dy.Dys=(meanSteps.Dy*steps.Dy/sum(steps)))%>%
      select(-steps,-medianSteps.Dy)%>%
      mutate(date = as.numeric(date))

    testSet <- scale(unTidyData)

    predict for all data using parameter found in training the classifier

    interpolatedTidyData <- knn(train = trainingSet,
                                test = testSet,
                                cl = observation$steps,
                                k = floor(1.5*sqrt(tidyDataSize)),
                                prob=TRUE)

    missingValues <- interpolatedTidyData[missingValuesIndicies]

> create a new dataset that is equal to the original dataset but with
> the missing data filled in.

    unTidyData$newSteps <- activity$steps
    unTidyData$newSteps[missingValuesIndicies] <- missingValues
    newTidyData <- unTidyData

    calculate the new summary statistics

    newTidyData <- group_by(newTidyData, date)%>%
      summarise(newMeanSteps.Dy = mean(newSteps),
                newMedianSteps.Dy=median(newSteps),
                newSteps.Dy = sum(newSteps),
                newMaxSteps.Dy=max(newSteps))%>%
      merge(newTidyData)%>%
      mutate(newMeanSteps.Dy.Dys=(newMeanSteps.Dy*newSteps.Dy/sum(newSteps)))

> create a new time series

newTidyDataXTS \<- xts(newTidyData ,order.by = newDts,unique = TRUE)

> make a histogram of the total number of steps taken each day

    print(sample(newTidyData$newSteps.Dy,10))

    ##  [1] 10600  5441 20427  5441   288 12116 12883 13460  8334 13646

> make a histogram of the ***total number of steps taken per day***

create extra margin room on the right for an axis

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-20-1.png)

> calculate and report ***the mean***

    print(sample(newTidyData$newMeanSteps.Dy,10))

    ##  [1] 53.5208333  1.0000000 46.1597222 35.2048611  0.1423611 53.5208333
    ##  [7] 42.0694444 44.3993056 35.2048611 50.2708333

> and ***the median***

    print(sample(newTidyData$newMedianSteps.Dy,10))

    ##  [1] 0 0 0 0 0 1 0 0 0 0

> of ***total number of steps taken in a day per day***

#### Do these values differ from the estimates from the first part of the assignment?

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-23-1.png)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-24-1.png)

#### What is the impact of inputing missing data on the estimates of the total daily number of steps?

> > *From the above graph it can be seen how the interpolated data is
> > shifted. The brown lines follow closely the black but is slightly
> > shifted in time. This is a direct resut of the k nearest neighbor
> > classification algorithm, employed to predict the missing values.*

> > *Additionally the day with the max average has not changed. Note the
> > greenish transparent line at day 50. The greenish color is caused by
> > the over lap of the data points from both series (blue and yellow).*

### Are there differences in activity patterns between weekdays and weekends?

#### Use the dataset with the filled-in missing values for this part.

> Create a new factor variable in the dataset with two levels â€“
> â€œweekdayâ€ and â€œweekendâ€

> indicating whether a given date is a weekday or weekend day.

    weekDayIndices = !(weekdays(newDts,TRUE)==c("Fri" ,"Sat" ,"Sun"))
    weekEndIndices = (weekdays(newDts,TRUE)==c("Fri" ,"Sat" ,"Sun"))

    Calculate the summary statistics

    newTidyData <- mutate(newTidyData,
                          dayClass = factor(        # create the weekday/end factor
                            1+(weekEndIndices),
                            labels = c("day","end"),
                            levels = c(1,2)))

    newTidyData.Wd <- newTidyData[weekDayIndices,]
    newTidyData.Wd <-
      group_by(newTidyData.Wd,dayClass)%>%
      summarise(newMeanSteps.Wd = mean(newSteps),  # calculate the weighted average of weekdays
                newMedianSteps.Wd = median(newSteps),
                newSteps.Wd = sum(newSteps),
                newMaxSteps.Wd = max(newSteps))%>%
      merge(newTidyData.Wd)%>%
      mutate(newMeanSteps.Wd.Wds=(newMeanSteps.Wd*newSteps.Wd/sum(newSteps)))

    newTidyData.We <- newTidyData[weekEndIndices,]
    newTidyData.We <-
      group_by(newTidyData.We,dayClass)%>%
      summarise(newMeanSteps.We = mean(newSteps),    # calculate the weighted average of weekends
                newMedianSteps.We=median(newSteps),
                newSteps.We = sum(newSteps),
                newMaxSteps.We=max(newSteps))%>%
      merge(newTidyData.We)%>%
      mutate(newMeanSteps.We.Wes=(newMeanSteps.We*newSteps.We/sum(newSteps)))

    newTidyDataXTS.Wd <- xts(newTidyData.Wd ,order.by = newDts[weekDayIndices],unique = TRUE)
    newTidyDataXTS.We <- xts(newTidyData.We ,order.by = newDts[weekEndIndices],unique = TRUE)

Make a panel plot containing a time series plot of the 5-minute interval
and the average number of steps taken, averaged across all weekday days
or weekend days.

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-26-1.png)

> > **There appears to be some difference in the activity partern based
> > on the weekend and weekday averages in the above graphs and show
> > here respectivley**

    head(newTidyDataXTS.We$newMeanSteps.We.Wes)

    ##                     newMeanSteps.We.Wes
    ## 2012-10-05 00:00:00 "34.22528"         
    ## 2012-10-05 00:05:00 "34.22528"         
    ## 2012-10-05 00:15:00 "34.22528"         
    ## 2012-10-05 00:30:00 "34.22528"         
    ## 2012-10-05 00:45:00 "34.22528"         
    ## 2012-10-05 01:00:00 "34.22528"

    head(newTidyDataXTS.Wd$newMeanSteps.Wd.Wds)

    ##                     newMeanSteps.Wd.Wds
    ## 2012-10-01 00:00:00 "32.35113"         
    ## 2012-10-01 00:05:00 "32.35113"         
    ## 2012-10-01 00:10:00 "32.35113"         
    ## 2012-10-01 00:15:00 "32.35113"         
    ## 2012-10-01 00:20:00 "32.35113"         
    ## 2012-10-01 00:25:00 "32.35113"

rmarkdown::render(input="PA1\_template.Rmd",output\_format="md\_document",output\_file
= "README.md")
rmarkdown::render(input="PA1\_template.Rmd",output\_format="html\_document",output\_file
= "README.md")
