#' ---
#' title: "Reproducible Research: Peer Assessment 1"
#' author: "grldsndrs"
#' date:
#' output:
#' html_document:
#' keep_md: true
#' ---
#'
#' ```{r echo=FALSE}
#' date()
#' ```

#' ## Loading and preprocessing the data
#' > _"Project Template"_ uses the defualt settings to load the csv
#' > file in the data folder of the working diriectory.
#'
#' ### Data cleaning
tidyData <- activity[complete.cases(activity),]
#' ### Data tidying
# create a time series by adding the intervals in minutes to the date
dts <- as.POSIXct(tidyData$date) + minutes(as.numeric(tidyData$interval ))
#' > choose appropriate classes for features
tidyData <- mutate(tidyData,
                   steps= as.numeric(steps),
                   date = as.factor(date),
                   interval=as.numeric(interval))
#' > create a time series
tidyDataXTS <- xts(tidyData ,order.by = dts,unique = TRUE)

#' ```{r echo=FALSE}
#' print(head(sample_n(tidyData,nrow(tidyData))))
#' ```

#' #### What is mean total number of steps taken per day?
#' > calculate the **_total number of steps taken per day_**
dys = endpoints(tidyDataXTS, 'days')
tidyData <- group_by(tidyData, date)%>%
  summarise(meanSteps.Dy = mean(steps),
            medianSteps.Dy=median(steps),
            steps.Dy = sum(steps),
            maxSteps.Dy=max(steps))%>%
  merge(tidyData)%>%
  mutate(meanSteps.Dy.Dys=(meanSteps.Dy*steps.Dy/sum(steps)))

tidyDataXTS <- xts(tidyData ,order.by = dts,unique = TRUE)


#' ```{r echo=TRUE}
#' print(sample(tidyData$steps.Dy,10))
#' ```
#' > make a histogram of the **_total number of steps taken per day_**
#' ```{r, echo=FALSE}
#' # create extra margin room on the right for an axis
#'  par(mar=c(5, 5, 4, 2) + 0.1,mgp=c(4,1,0))
#'  plot.xts(tidyDataXTS$steps.Dy, type = 'h',# histogram
#'    major.ticks = 'days',
#'    minor.ticks = FALSE,
#'    las = 2,
#'    xaxt = "n",
#'          xlab = "day",
#'          ylab = "Total Number of Steps in day",
#'         main = "Counts of Data Acquisitioned Steps")
#' axis(1, at=dts[dys],labels=1:(length(dys)-1),las=1)
#' ```
#'
#' > calculate and report **_the mean_**
#' ```{r echo=TRUE}
#' print(sample(tidyData$meanSteps.Dy,10))
#' ```
#'  > and **_the median_**
#' ```{r echo=TRUE}
#' print(sample(tidyData$medianSteps.Dy,10))
#' ```
#' > of **_total number of steps taken in a day per day_**
#'
#' #### What is the average daily activity pattern?
#' > Make a time series plot of the 5-minute interval
#' ```{r, echo=FALSE}
#' par(mar=c(5, 5, 5, 12) + 0.1,mgp=c(4,1,0))
#' plot.xts(
#'    tidyDataXTS$meanSteps.Dy.Dys,
#'   major.ticks = 'days',
#'   minor.ticks = FALSE,
#'   col = "black",
#'   las = 2,
#'   xaxt = "n",
#'   type = "l",# line
#'   xlab = "day",
#'   ylab = "average number of steps taken in a day",
#'   main = "Average Daily Activity Pattern"
#' )
#' par(new=TRUE)
#' plot.xts(
#'  tidyDataXTS$maxSteps.Dy,
#'   major.ticks = 'days',
#'   minor.ticks = FALSE,
#'   col = "gray",
#'   las = 2,
#'   yaxt = "n",
#'   xaxt = "n",
#'   type = "l",# line
#'   main =""
#' )
#' axis(4, at=quantile(tidyData$maxSteps.Dy,names = FALSE),las=1)
#' mtext("max number of steps on day",side=4,line=3)
#' axis(1, at=dts[dys],labels=1:(length(dys)-1),las=1)
#' par( xpd = TRUE ,mar = par()$mar + c(0,0,0,7))
#' legend(x = "bottomright", inset = c(-0.7,1.1),
#'       c("Interal has max num steps", "Avg num steps/day", "Max steps/day"),
#'       col = c("blue","black", "gray"),
#'       lwd = c(3,2, 2),
#'       bty ="n")
#' par(xpd = FALSE,mar=c(5, 5, 5, 12) + 0.1,mgp=c(4,1,0))
#' maxNumStepsValue = max(tidyData$steps)
#' maxNumStepsDateTime = dts[tidyData$steps==maxNumStepsValue]
#' abline( v = maxNumStepsDateTime, col = "blue", ,lwd = 3)
#' points(maxNumStepsDateTime, maxNumStepsValue, cex= 1, pch =16,col="red")
#' ```
#'
#' #### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
#'
#' > **_The interval is indicated by the red dot on the above plot._**
#' > It is calculated:
#' ```{r eval=TRUE}
#' maxNumStepsValue = max(tidyData$steps)
#' intervalOfMax = tidyData$interval[tidyData$steps==maxNumStepsValue]
#' maxNumStepsDateTime = dts[tidyData$steps==maxNumStepsValue]
#' ```

#' ```{r echo=FALSE}
#' maxNumStepsValue
#' intervalOfMax
#' print(format(maxNumStepsDateTime, "%m/%d/%y %H:%M"))
#' ```

#' ```{r eval=TRUE}
#' ```
#' ##Inputing missing values

#' >>Note that there are a number of days/intervals where there are missing values.
#'  The presence of missing days may introduce bias into some calculations or summaries of the data.
#'
#' > calculate and report the total number of missing values in the dataset
summary(activity)

#' > devise a strategy for filling in all of the missing values in the dataset.
#'
#' >> Use a k nearst neighbors algorithm to predict the missing values based on the known.
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
# check accuracy of predictions
sum(as.numeric(tidyData$steps[testSetIndicies]==testPredictions))/length(testPredictions)
#' >> 70% good enough for the purposes here. \n
#'
#' can use paramerter setting to predict the missing values
missingValuesIndicies = is.na(activity$steps)

# get new raw data
unTidyData <- activity
# get new time series
newDts <- as.POSIXct(unTidyData$date) + minutes(as.numeric(unTidyData$interval ))
# switch up classes for processing
unTidyData <- mutate(unTidyData,
                     steps = as.numeric(steps),
                     date = as.factor(date),
                     interval = as.numeric(interval))

# initialize the missing values to the mean of the day if it exists
# or the mean of the data set if not
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

# prep data to predict steps with the nearest neighbor classifier
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

# predict for all data using parameter found in training the classifier
interpolatedTidyData <- knn(train = trainingSet,
                            test = testSet,
                            cl = observation$steps,
                            k = floor(1.5*sqrt(tidyDataSize)),
                            prob=TRUE)

missingValues <- interpolatedTidyData[missingValuesIndicies]

#' > create a new dataset that is equal to the original dataset but with the missing data filled in.

unTidyData$newSteps <- activity$steps
unTidyData$newSteps[missingValuesIndicies] <- missingValues
newTidyData <- unTidyData

# calculate the new summary statistics
newTidyData <- group_by(newTidyData, date)%>%
  summarise(newMeanSteps.Dy = mean(newSteps),
            newMedianSteps.Dy=median(newSteps),
            newSteps.Dy = sum(newSteps),
            newMaxSteps.Dy=max(newSteps))%>%
  merge(newTidyData)%>%
  mutate(newMeanSteps.Dy.Dys=(newMeanSteps.Dy*newSteps.Dy/sum(newSteps)))

#' > create a new time series
newTidyDataXTS <- xts(newTidyData ,order.by = newDts,unique = TRUE)
#'
#' > make a histogram of the total number of steps taken each day
#' ```{r echo=TRUE}
#' print(sample(newTidyData$newSteps.Dy,10))
#' ```
#' > make a histogram of the **_total number of steps taken per day_**
#' ```{r, echo=FALSE}
#' # create extra margin room on the right for an axis
#'  par(mar=c(5, 5, 4, 2) + 0.1,mgp=c(4,1,0))
#'  plot.xts(newTidyDataXTS$newSteps.Dy, type = 'h',# histogram
#'    major.ticks = 'days',
#'    minor.ticks = FALSE,
#'    las = 2,
#'    xaxt = "n",
#'          xlab = "day",
#'          ylab = "Total Number of Steps in day",
#'         main = "Counts of Data Acquisitioned Steps")
#' axis(1, at=newDts[dys],labels=1:(length(dys)-1),las=1)
#' ```
#'
#' > calculate and report **_the mean_**
#' ```{r echo=TRUE}
#' print(sample(newTidyData$newMeanSteps.Dy,10))
#' ```
#'  > and **_the median_**
#' ```{r echo=TRUE}
#' print(sample(newTidyData$newMedianSteps.Dy,10))
#' ```
#' > of **_total number of steps taken in a day per day_**
#'
#' #### Do these values differ from the estimates from the first part of the assignment?
#' ```{r, echo=FALSE}
#' par(mar=c(5, 4, 4, 2) + 0.1,mgp=c(3,1,0))
#' plot(
#'   newTidyData$meanSteps.Dy,newTidyData$newMeanSteps.Dy,
#'   col = alpha("black", .5),
#'   las = 2,
#'   type = "p",# points
#'   xlab = "",
#'   ylab = "",
#'   xaxt = "n",
#'   yaxt = "n",
#'   main = "Compare the means and medians"
#' )
#' par(new=TRUE)
#' plot(
#'   newTidyData$newMedianSteps.Dy,
#'   col = alpha("blue", .01),
#'   las = 2,
#'   type = "p",# points
#'   xlab = "Omited NAs",
#'   ylab = "Interpolated NAs",
#'   main = ""
#' )
#' legend(x = "topleft",
#'        c("Interpolated Mean vs NAs omitted Mean",
#'        "Interpolated median"),
#'        col = c(alpha("black", 1),alpha("blue", 1)),
#'        lwd = c(3),
#'        bty ="n")
#' ```
#' ```{r, echo=FALSE}
#' par(mar=c(5, 4, 4, 2) + 0.1,mgp=c(3,1,0))
#' plot.xts(
#'   tidyDataXTS$meanSteps.Dy.Dys,
#'   major.ticks = 'days',
#'   minor.ticks = FALSE,
#'   col = alpha("black", .75),
#'   las = 2,
#'   xaxt = "n",
#'   type = "l",# line
#'   xlab = "day",
#'   ylab = "average number of steps taken in a day",
#'   main = "Average Daily Activity Pattern",
#'   auto.grid = FALSE
#' )
#'
#' axis(1, at=dts[dys],labels=1:(length(dys)-1),las=1)
#' par(new=TRUE)
#' plot.xts(
#'   newTidyDataXTS$newMeanSteps.Dy.Dys,
#'   major.ticks = 'days',
#'   minor.ticks = FALSE,
#'   col = alpha("chocolate", .5),
#'   las = 2,
#'   xaxt = "n",
#'   yaxt = "n",
#'   type = "l",# line
#'   main ="",
#'   auto.grid = FALSE
#' )
#' legend(x = "topleft",
#'        c("Day which has max num steps",
#'          "Avg num steps/day",
#'          "Interpolated day which has max num steps",
#'          "Interpolated Avg num steps/day"),
#'        col = c(alpha("blue", 1),
#'                alpha("black", .75),
#'                alpha("yellow", 1),
#'                alpha("chocolate", .5)),
#'        lwd = c(6,5,6,5),
#'        bty ="n")
#' maxNumStepsValue = max(tidyData$meanSteps.Dy.Dys)
#' maxNumStepsDateTime = dts[tidyData$meanSteps.Dy.Dys==maxNumStepsValue]
#' abline( v = maxNumStepsDateTime, col = alpha("blue", .03) ,lwd = .1)
#'
#' newMaxNumStepsValue = max(newTidyData$newMeanSteps.Dy.Dys)
#' newIntervalOfMax = newTidyData$interval[newTidyData$newMeanSteps.Dy.Dys==newMaxNumStepsValue]
#' newMaxNumStepsDateTime = newDts[newTidyData$newMeanSteps.Dy.Dys==newMaxNumStepsValue]
#' abline( v = newMaxNumStepsDateTime, col = alpha("yellow", .03),lwd = .1)
#' ```
#'
#' #### What is the impact of inputing missing data on the estimates of the total daily number of steps?
#' >> _From the above graph it can be seen how the interpolated data is shifted.
#' The brown lines follow closely the black but is slightly shifted in time.
#' This is a direct resut of the k nearest neighbor classification algorithm,
#'  employed to predict the missing values._
#'
#'  >>_Additionally the day with the max average has not changed.
#'    Note the greenish  transparent line at day 50. The greenish color is caused by
#'    the over lap of the data points from both series (blue and yellow)._
#' #### Are there differences in activity patterns between weekdays and weekends?
#' #### Use the dataset with the filled-in missing values for this part.
#' > Create a new factor variable in the dataset with two levels – “weekday” and “weekend”
#' > indicating whether a given date is a weekday or weekend day.

weekDayIndices = !(weekdays(newDts,TRUE)==c("Fri" ,"Sat" ,"Sun"))
weekEndIndices = (weekdays(newDts,TRUE)==c("Fri" ,"Sat" ,"Sun"))
# Calculate the summary statistics
newTidyData <- mutate(newTidyData,
                      dayClass = factor(        # create the weekday/end factor
                        1+(weekEndIndices),
                        labels = c("day","end"),
                        levels = c(1,2)))

newTidyData <- group_by(newTidyData,dayClass)%>%
  summarise(newMeanSteps.Wd = mean(newSteps[weekDayIndices]),  # calculate the weighted average of weekdays
            newMedianSteps.Wd=median(newSteps[weekDayIndices]),
            newSteps.Wd = sum(newSteps[weekDayIndices]),
            newMaxSteps.Wd=max(newSteps[weekDayIndices]))%>%
  merge(newTidyData)%>%
  mutate(newMeanSteps.Wd.Wds=(newMeanSteps.Wd*newSteps.Wd/sum(newSteps[weekDayIndices])))

newTidyData <- group_by(newTidyData,dayClass)%>%
  summarise(newMeanSteps.We = mean(newSteps[weekEndIndices]),    # calculate the weighted average of weekends
            newMedianSteps.We=median(newSteps[weekEndIndices]),
            newSteps.We = sum(newSteps[weekEndIndices]),
            newMaxSteps.We=max(newSteps[weekEndIndices]))%>%
  merge(newTidyData)%>%
  mutate(newMeanSteps.We.Wes=(newMeanSteps.We*newSteps.We/sum(newSteps[weekEndIndices])))

newTidyDataXTS <- xts(newTidyData ,order.by = newDts,unique = TRUE)
#' Make a panel plot containing a time series plot of the 5-minute interval
#' and the average number of steps taken,
#' averaged across all weekday days or weekend days.
#' ```{r, echo=FALSE}
#' par(mar=c(5, 4, 4, 2) + 0.1,mgp=c(3,1,0),mfrow=c(2,1))
#' plot.xts(
#'   newTidyDataXTS$newMeanSteps.Wd.Wds,
#'   major.ticks = 'days',
#'   minor.ticks = FALSE,
#'   col = alpha("black", .75),
#'   las = 2,
#'   xaxt = "n",
#'   type = "l",# line
#'   xlab = "day",
#'   ylab = "average number of steps taken in a weekday",
#'   main = "Average Weekly Activity Pattern",
#'   auto.grid = FALSE
#' )
#'
#' axis(1, at=newDts[dys],labels=1:(length(dys)-1),las=1)
#' newMaxNumStepsValue.Wd = max(newTidyData$new$MeanSteps.Wd.Wds)
#' newMaxNumStepsDateTime.Wd = newDts[newTidyData$newMeanSteps.Wd.Wds==newMaxNumStepsValue.Wd]
#' abline( v = newMaxNumStepsDateTime.Wd, col = alpha("blue", .03) ,lwd = .1)
#' plot.xts(
#'   newTidyDataXTS$newMeanSteps.We.Wes,
#'   major.ticks = 'days',
#'   minor.ticks = FALSE,
#'   col = alpha("chocolate", .5),
#'   las = 2,
#'   xaxt = "n",
#'   yaxt = "n",
#'   type = "l",# line
#'   main ="",
#'   auto.grid = FALSE
#' )
#' legend(x = "topleft",
#'        c("Week which has max num steps",
#'          "Avg num steps/weekdays",
#'          "Weekend which has max num steps",
#'          "Weekends Avg num steps/day"),
#'        col = c(alpha("blue", 1),
#'                alpha("black", .75),
#'                alpha("yellow", 1),
#'                alpha("chocolate", .5)),
#'        lwd = c(6,5,6,5),
#'        bty ="n")
#'
#' newMaxNumStepsValue.We = max(newTidyData$newMeanSteps.We.Wes)
#' newIntervalOfMax.We = newTidyData$interval[newTidyData$newMeanSteps.We.Wes==newMaxNumStepsValue.We]
#' newMaxNumStepsDateTime.We = newDts[newTidyData$newMeanSteps.We.Wes==newMaxNumStepsValue.We]
#' abline( v = newMaxNumStepsDateTime.We, col = alpha("yellow", .03),lwd = .1)
#' ```
# #
# library('ProjectTemplate')
# load.project()
# # And then goes on to do something original with the data:

##   plot1 <- ggplot(first.letter.counts, aes(x = V1)) + geom_density()
## ggsave(file.path('graphs', 'plot1.pdf'))
#
## plot2 <- ggplot(second.letter.counts, aes(x = V1)) + geom_density()
## ggsave(file.path('graphs', 'plot2.pdf'))
