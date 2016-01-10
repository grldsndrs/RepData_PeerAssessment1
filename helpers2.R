par(mar=c(5, 4, 4, 2) + 0.1,mgp=c(3,1,0))
plot.xts(
  tidyDataXTS$meanSteps.Dy.Dys,
  major.ticks = 'days',
  minor.ticks = FALSE,
  col = alpha("black", .75),
  las = 2,
  xaxt = "n",
  type = "l",# line
  xlab = "day",
  ylab = "average number of steps taken in a day",
  main = "Average Daily Activity Pattern",
  auto.grid = FALSE
)

axis(1, at=dts[dys],labels=1:(length(dys)-1),las=1)
par(new=TRUE)
plot.xts(
  newTidyDataXTS$newMeanSteps.Dy.Dys,
  major.ticks = 'days',
  minor.ticks = FALSE,
  col = alpha("chocolate", .5),
  las = 2,
  xaxt = "n",
  yaxt = "n",
  type = "l",# line
  main ="",
  auto.grid = FALSE
)
legend(x = "topleft",
       c("Day which has max num steps",
         "Avg num steps/day",
         "Interpolated day which has max num steps",
         "Interpolated Avg num steps/day"),
       col = c(alpha("blue", 1),
               alpha("black", .75),
               alpha("yellow", 1),
               alpha("chocolate", .5)),
       lwd = c(6,5,6,5),
       bty ="n")
maxNumStepsValue = max(tidyData$meanSteps.Dy.Dys)
maxNumStepsDateTime = dts[tidyData$meanSteps.Dy.Dys==maxNumStepsValue]
abline( v = maxNumStepsDateTime, col = alpha("blue", .03) ,lwd = .1)

newMaxNumStepsValue = max(newTidyData$newMeanSteps.Dy.Dys)
newIntervalOfMax = newTidyData$interval[newTidyData$newMeanSteps.Dy.Dys==newMaxNumStepsValue]
newMaxNumStepsDateTime = newDts[newTidyData$newMeanSteps.Dy.Dys==newMaxNumStepsValue]
abline( v = newMaxNumStepsDateTime, col = alpha("yellow", .03),lwd = .1)

par(mar=c(5, 4, 4, 2) + 0.1,mgp=c(3,1,0))
plot(
  newTidyData$meanSteps.Dy,newTidyData$newMeanSteps.Dy,
  col = alpha("black", .5),
  las = 2,
  type = "p",# points
  xlab = "Old Mean",
  ylab = "New Mean",
  main = "Compare the two means"
)
par(new=TRUE)
plot(
  newTidyData$newMedianSteps.Dy,
  col = alpha("blue", .5),
  las = 2,
  type = "p",# points
  xlab = "Omited NAs",
  ylab = "Interpolated NAs",
  main = "Compare the two means and medians"
)
legend(x = "topleft",
       c("Interpolated Mean/Median v.s Dimension Reduced Mean/Median (NAs omitted)"),
       col = alpha("blue", 1),
       lwd = c(3),
       bty ="n")

par(mar=c(5, 4, 4, 2) + 0.1,mgp=c(3,1,0),mfrow=c(2,1))
plot.xts(
  newTidyDataXTS$newMeanSteps.Wd.Wds,
  major.ticks = 'days',
  minor.ticks = FALSE,
  col = alpha("black", .75),
  las = 2,
  xaxt = "n",
  type = "l",# line
  xlab = "day",
  ylab = "average number of steps taken in a weekday",
  main = "Average Weekly Activity Pattern",
  auto.grid = FALSE
)

axis(1, at=newDts[dys],labels=1:(length(dys)-1),las=1)
newMaxNumStepsValue.Wd = max(newTidyData$new$MeanSteps.Wd.Wds)
newMaxNumStepsDateTime.Wd = newDts[newTidyData$newMeanSteps.Wd.Wds==newMaxNumStepsValue.Wd]
abline( v = newMaxNumStepsDateTime.Wd, col = alpha("blue", .03) ,lwd = .1)
plot.xts(
  newTidyDataXTS$newMeanSteps.We.Wes,
  major.ticks = 'days',
  minor.ticks = FALSE,
  col = alpha("chocolate", .5),
  las = 2,
  xaxt = "n",
  yaxt = "n",
  type = "l",# line
  main ="",
  auto.grid = FALSE
)
legend(x = "topleft",
       c("Week which has max num steps",
         "Avg num steps/weekdays",
         "Weekend which has max num steps",
         "Weekends Avg num steps/day"),
       col = c(alpha("blue", 1),
               alpha("black", .75),
               alpha("yellow", 1),
               alpha("chocolate", .5)),
       lwd = c(6,5,6,5),
       bty ="n")

newMaxNumStepsValue.We = max(newTidyData$newMeanSteps.We.Wes)
newIntervalOfMax.We = newTidyData$interval[newTidyData$newMeanSteps.We.Wes==newMaxNumStepsValue.We]
newMaxNumStepsDateTime.We = newDts[newTidyData$newMeanSteps.We.Wes==newMaxNumStepsValue.We]
abline( v = newMaxNumStepsDateTime.We, col = alpha("yellow", .03),lwd = .1)
