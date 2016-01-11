`## [1] "Sun Jan 10 23:56:17 2016"`

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
    ## 10889    74 2012-11-13     1920
    ## 10547     0 2012-11-12     1450
    ## 6307     21 2012-10-24     2130
    ## 11399    33 2012-11-16     1350
    ## 1460      0 2012-10-07      135
    ## 2342      0 2012-10-11      305

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

    ##  [1]  3219  7047 10183 12883 12426 15414 11829 11015 12426 10571

> make a histogram of the ***total number of steps taken per day***

![](README_files/figure-markdown_strict/unnamed-chunk-8-1.png)

> calculate and report ***the mean***

    print(sample(tidyData$meanSteps.Dy,10))

    ##  [1] 39.7847222 47.3819444 49.7881944 46.7083333 60.3541667 23.5347222
    ##  [7]  0.1423611 36.0937500 35.3576389 36.7048611

> and ***the median***

    print(sample(tidyData$medianSteps.Dy,10))

    ##  [1] 0 0 0 0 0 0 0 0 0 0

> of ***total number of steps taken in a day per day***

#### What is the average daily activity pattern?

> Make a time series plot of the 5-minute interval

![](README_files/figure-markdown_strict/unnamed-chunk-11-1.png)

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

rmarkdown::render(input="PA1\_template.Rmd",output\_format="md\_document",output\_file
= "README.md")

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

Loading and preprocessing the data
----------------------------------

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

    library(ProjectTemplate)
    load.project()
    tidyData <- activity[complete.cases(activity),]
    # convert date to a time series by adding the intervals in minutes
    dts <- as.POSIXct(tidyData$date) + minutes(as.numeric(tidyData$interval ))
    # reshape data into a 1 feature time series
    tidyData <- select(tidyData ,-date,-interval)%>%
    xts(order.by = dts,unique = TRUE)

You can also embed plots, for example:

![](README_files/figure-markdown_strict/unnamed-chunk-15-1.png)

What is mean total number of steps taken per day?
-------------------------------------------------

What is the average daily activity pattern?
-------------------------------------------

Imputing missing values
-----------------------

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------
