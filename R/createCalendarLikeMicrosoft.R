#' Create a calendar with weeks that match what the WEEKNUM([Date]) and DATEPART(ww, [Date]) functions return in Excel and sQL, respectively.
#'
#' This function takes a starting year and a sub-group of year (i.e. week, month, quarter) and returns every date between that start year and today with a DateGroup.
#'
#' @param startYear is a numeric value that is a real year
#' @param yearSubGroup is a sub-group of a year, i.e. 'Week', 'Month', or 'Quarter'
#' @return a data frame of dates starting at Jan 1 of the startYear and ending today, which the corresponding year and DateGroup (e.g. 2015-01 for Week 1 of 2015 if yearSubGroup = 'Week')
#' @author Aimie Faucett
#' @details
#' This function takes in a startYear and yearSubGroup and returns a data frame containing all the dates from the start of the startYear
#' through today along with the year and yearSubGroup number. If the yearSubGroup is 'Week', then the function will return week numbers consistent
#' with Microsoft's Excel WEEKNUM and SQL DATEPART(ww, ) functions.
#' @export
#' @importFrom lubridate month year

createCalendarLikeMicrosoft <- function(startYear, yearSubGroup) {

  thisYear <- year(Sys.Date())
  datesToInclude <- data.frame(Date = do.call(c, lapply(startYear:thisYear, function(year) seq(as.Date(paste0(year,'/1/1')), as.Date(paste0(year,'/12/31')), 'day'))))
  datesToInclude[,'Year'] <- year(datesToInclude[,'Date'])
  # datesToInclude <- datesToInclude[datesToInclude[,'Date'] <= Sys.Date(), ]

  years <- as.character(unique(datesToInclude[,'Year']))

  outFrame <- c()
  for(i in 1:length(years)) {

    subFrame <- datesToInclude[datesToInclude[,'Year'] == years[i], ]

    if(yearSubGroup == 'Week') {

      subFrame[,'DayOfWeek'] <- format(subFrame[,'Date'],'%w')
      subFrame[,'Index'] <- seq(1, length(subFrame[,'Date']), 1)
      cutBreaks <- which(subFrame[,'DayOfWeek'] == '0')
      cutLabels <- seq(1, 53, 1)

      if(min(cutBreaks) == 1) {

        subFrame[,'Week'] <- cut(subFrame[,'Index'], breaks = c(cutBreaks, 366), include.lowest = TRUE, right = FALSE, labels = cutLabels)

      } else {

        if(length(levels(cut(subFrame[,'Index'], breaks = c(1, cutBreaks, 366)))) < length(cutLabels)) {

          subFrame[,'Week'] <- cut(subFrame[,'Index'], breaks = c(1, cutBreaks, 366), include.lowest = TRUE, right = FALSE, labels = cutLabels[1:(length(cutLabels)-1)])
        } else {

          subFrame[,'Week'] <- cut(subFrame[,'Index'], breaks = c(1, cutBreaks, 366), include.lowest = TRUE, right = FALSE, labels = cutLabels)
        }
      }

      subFrame[,'Week'] <- as.numeric(as.character(subFrame[,'Week']))
      subFrame[,'DateGroup'] <- with(subFrame, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))
    }

    else if(yearSubGroup == 'Month') {

      subFrame[,'Month'] <- month(subFrame[,'Date'])
      subFrame[,'DateGroup'] <- with(subFrame, ifelse(Month < 10, paste(Year, Month, sep='-0'), paste(Year, Month, sep='-')))
    }

    else if(yearSubGroup == 'Quarter') {

      subFrame[,'Month'] <- month(subFrame[,'Date'])
      subFrame[,'Quarter'] <- with(subFrame, ifelse(Month < 4, 1,
                                                    ifelse(Month < 7, 2,
                                                           ifelse(Month < 10, 3, 4))))
      subFrame[,'DateGroup'] <- with(subFrame, paste(Year, Quarter, sep='-0'))
    }

    else { stop('This function does not recognize the yearSubGroup parameter. It currently only takes one of "Week", "Month", or "Quarter".') }

    outFrame <- rbind(outFrame, subFrame)
  }

  outFrame <- outFrame[outFrame[,'Date'] <= Sys.Date(), c('Date','Year',yearSubGroup,'DateGroup')]
  return(outFrame)
}
