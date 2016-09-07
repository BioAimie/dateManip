#' Create weeks similar to CDC reporting periods
#'
#' This function takes in a calendar data frame that has an entry for every date since a starting point up through today and
#' returns a data frame that has a week number matching CDC reporting for each assoicated date.
#'
#' @param calFrame a data frame containing a column of consecutive dates
#' @return a data frame that contains the date and the corresponding CDC reporting week
#' @author Aimie Faucett
#' @details
#' This function takes a calFrame and returns a data.frame that contains the date with its corresponding CDC reporting week
#' @export

makeEvenWeeks <- function(calFrame) {

  calendarFrame[,'Year'] <- year(calendarFrame[,'Date'])
  calendarFrame[,'DayOfYear'] <- yday(calendarFrame[,'Date'])

  years <- unique(calendarFrame[,'Year'])
  leapYears <- years[(years %% 4 == 0)]

  outFrame <- c()

  for(i in 1:length(years)) {

    yearFrame <- calendarFrame[calendarFrame[,'Year'] == years[i], ]

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
