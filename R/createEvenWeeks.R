#' Create weeks that are relatively even such that there are 52 weeks in the year
#'
#' This function takes in a calendar data frame that has an entry for every date since a starting point up through today and
#' returns a data frame that has a week number 1-52 that are approximately even starting with Jan 1 as Week 1, day 1.
#'
#' @param calFrame a data frame containing a column of consecutive dates
#' @return a data frame that contains the date and the corresponding even week
#' @author Aimie Faucett
#' @details
#' This function takes a calFrame and returns a data.frame that contains the date with its corresponding CDC reporting week
#' @export

createEvenWeek <- function(calFrame) {

  calFrame[,'Year'] <- year(calFrame[,'Date'])
  calFrame[,'DayOfYear'] <- yday(calFrame[,'Date'])

  years <- unique(calFrame[,'Year'])
  leapYears <- years[(years %% 4 == 0)]

  outFrame <- c()

  for(i in 1:length(years)) {

    yearFrame <- calFrame[calFrame[,'Year'] == years[i], ]

    if(!(years[i] %in% leapYears)) {

      weekBreaks <- seq(1, 365, 7)
      yearFrame[,'Week'] <- cut(yearFrame[,'DayOfYear'], weekBreaks, include.lowest = TRUE, right = TRUE, labels = seq(1, 52, 1))
      outFrame <- rbind(outFrame, yearFrame)

    }
    else {

      weekBreaks <- seq(1, 365, 7)
      weekBreaks <- c(weekBreaks[1:52], 366)
      yearFrame[,'Week'] <- cut(yearFrame[,'DayOfYear'], weekBreaks, include.lowest = TRUE, right = TRUE, labels = seq(1, 52, 1))
      outFrame <- rbind(outFrame, yearFrame)
    }
  }

  outFrame[,'Week'] <- as.numeric(outFrame[,'Week'])
  outFrame[,'YearWeek'] <- with(outFrame, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))
  return(outFrame)
}
