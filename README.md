`## [1] "Sun Jan 10 22:58:20 2016"`

Loading and preprocessing the data
----------------------------------

*"Project Template"* uses the defualt settings to 'load.project()' the
csv file in the data folder of the working diriectory.

### Data cleaning

    tidyData <- activity[complete.cases(activity),]

### Data tidying

create a time series by adding the intervals in minutes to the date

dts \<-
as.POSIXct(tidyData*d**a**t**e*) + *m**i**n**u**t**e**s*(*a**s*.*n**u**m**e**r**i**c*(*t**i**d**y**D**a**t**a*interval))

> **choose appropriate classes for features**

    tidyData <- mutate(tidyData,
                       steps= as.numeric(steps),
                       date = as.factor(date),
                       interval=as.numeric(interval))

> create a time series

    tidyDataXTS <- xts(tidyData ,order.by = dts,unique = TRUE)

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

![](README_files/figure-markdown_strict/unnamed-chunk-6-1.png)

What is mean total number of steps taken per day?
-------------------------------------------------

What is the average daily activity pattern?
-------------------------------------------

Imputing missing values
-----------------------

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------
