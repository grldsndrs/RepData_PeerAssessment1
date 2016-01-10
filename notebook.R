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
tidyData <- mutate(tidyData,steps= as.numeric(steps),date = as.factor(date),interval=as.numeric(interval))
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
#' par(mar=c(5, 5, 4, 5) + 0.1,mgp=c(4,1,0))
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
#' legend(x = "topleft",
#'       c("Contains the maximum number of steps", "Avg num steps/day", "Maximum steps in day"),
#'       col = c("blue","black", "gray"),
#'       lwd = c(3,2, 2),
#'       bty ="n")
#' maxNumStepsValue = max(tidyData$steps)
#' maxNumSteps = dts[tidyData$steps==maxNumStepsValue]
#' abline( v = maxNumSteps, col = "blue", ,lwd = 3)
#' points(maxNumSteps, maxNumStepsValue, cex= 1, pch =16,col="red")
#' ```
#'
#' #### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
#'
#' > **_The interval is indicated by the red dot on the above plot._**
#' > It is calculated:
#' ```{r eval=FALSE}
#' maxNumStepsValue = max(tidyData$steps)
#' maxNumSteps = dts[tidyData$steps==maxNumStepsValue]
#' ```

#' ```{r echo=FALSE}
#' print(format(maxNumSteps, "%m/%d/%y %H:%M"))
#' ```
#' ##Imputing missing values

#' >>Note that there are a number of days/intervals where there are missing values.
#'  The presence of missing days may introduce bias into some calculations or summaries of the data.
#'
#' > calculate and report the total number of missing values in the dataset.
#'
#' > devise a strategy for filling in all of the missing values in the dataset.
#' >> The strategy does not need to be sophisticated. For example,
#' you could use the mean/median for that day,
#' or the mean for that 5-minute interval, etc.
#'
#' > create a new dataset that is equal to the original dataset but with the missing data filled in.
#'
#' > make a histogram of the total number of steps taken each day
#'
#' > calculate and report the mean and median total number of steps taken per day.
#'
#' #### Do these values differ from the estimates from the first part of the assignment?
#'
#' #### What is the impact of imputing missing data on the estimates of the total daily number of steps?

#' #### Are there differences in activity patterns between weekdays and weekends?
# #
# library('ProjectTemplate')
# load.project()
# # And then goes on to do something original with the data:

##   plot1 <- ggplot(first.letter.counts, aes(x = V1)) + geom_density()
## ggsave(file.path('graphs', 'plot1.pdf'))
#
## plot2 <- ggplot(second.letter.counts, aes(x = V1)) + geom_density()
## ggsave(file.path('graphs', 'plot2.pdf'))
