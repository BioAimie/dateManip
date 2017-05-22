#' Find the start date in the format of a character DateGroup using today's DateGroup as the starting point.
#'
#' The function sequences DateGroups from last date to today, incrementing by -1, and keeps the number of DateGroup units as specified by the user.
#'
#' @param msCalFrame is a data frame (non-sparse) of all dates and DateGroups, an output of \code{createCalendarLikeMicrosoft} function
#' @param yearSubGroup is a sub-group of a year, i.e. 'Week', 'Month', or 'Quarter'
#' @param timeUnitsToKeep is a numeric value indicating how many of DateGroup entries to keep (incremented by 1 backwards from today's DateGroup)
#' @param rollPeriods is a numeric value indicating how many additional periods should be included in case the user wants to find a rolling sum, the default is 0.
#' @param keepPeriods is a numeric value indicating how many periods to keep such that the limit at calculation is the same as what it would have been when the period was current.
#' @return a character string DateGroup at timeUnitsToKeep + rollPeriods prior (e.g. 2016-01 if today's DateGroup is 2016-10 and timeUnitsToKeep = 9 and rollPeriods = 0)
#' @author Aimie Faucett
#' @details
#' This function takes in msCalFrame, yearSubGroup, timeUnitsToKeep, and rollPeriods. The unique msCalFrame DateGroups are found and then sequenced in descending order. The number of entries
#' kept is specified by timeUnitsToKeep + rollPeriods. The function returns the startDate (character string) as determined by the timeUnitsToKeep +
#' rollPeriods prior to the most current DateGroup.
#' @export

findStartDate <- function(msCalFrame, yearSubGroup, timeUnitsToKeep, rollPeriods = 0 , keepPeriods) {


  timeUnitsToKeep <- timeUnitsToKeep + keepPeriods
  uniqueCal <- unique(msCalFrame[,c('Year', yearSubGroup, 'DateGroup')])
  uniqueCal[,'Index'] <- seq(length(uniqueCal[,'Year']), 1, -1)

  if(rollPeriods > 0) {

    timeUnitsToKeep <- timeUnitsToKeep + rollPeriods - 1
  }

  if(timeUnitsToKeep > max(uniqueCal[,'Index'])) {

    stop('The timeUnitsToKeep parameter suggests keeping more periods than possible given the msCalFrame parameter.')
  }

  outFrame <- uniqueCal[uniqueCal[,'Index'] <= timeUnitsToKeep, c('Year', yearSubGroup, 'DateGroup')]
  startDate <- min(outFrame[,'DateGroup'])
  return(startDate)
}
