#' Create EPI weeks
#'
#' This function takes in a calendar data frame that has an entry for every date since a starting point up through today and
#' returns a data frame that has an EpiWeek and EpiYear along with the Date.
#'
#' @param calFrame a data frame containing a column of consecutive dates produced using createCalendarLikeMicrosoft with 'Week' as the yearSubGroup
#' @return a data frame that contains the date, EPI year and EPI week
#' @author Aimie Faucett
#' @details
#' This function takes a calFrame and returns a data.frame that contains the date with its corresponding EPI reporting week
#' @export

transformToEpiWeeks <- function(calFrame) {

  calFrame[,'DayOfYear'] <- yday(calFrame[,'Date'])
  calFrame[,'DayOfWeek'] <- wday(calFrame[,'Date'])

  years <- unique(calFrame[,'Year'])

  epiFrame <- do.call(rbind, lapply(1:length(years), function(x) data.frame(calFrame[calFrame$Year==years[x], ], FirstDayOfYear = calFrame[calFrame$Year==years[x] & calFrame$DayOfYear==min(calFrame[calFrame$Year==years[x],'DayOfYear']), 'DayOfWeek'], LastDayOfYear = calFrame[calFrame$Year==years[x] & calFrame$DayOfYear==max(calFrame[calFrame$Year==years[x],'DayOfYear']), 'DayOfWeek'])))

  epiFrame[leap_year(epiFrame$Year) & epiFrame$DayOfYear >= 363 & epiFrame$LastDayOfYear <= 3, 'EpiWeek'] <- 1
  epiFrame[epiFrame$DayOfYear >= 362 & epiFrame$LastDayOfYear <= 3, 'EpiWeek'] <- 1
  epiFrame[is.na(epiFrame$EpiWeek) & epiFrame$DayOfYear <= 4 & epiFrame$FirstDayOfYear <= 4, 'EpiWeek'] <- 1
  epiFrame[is.na(epiFrame$EpiWeek) & epiFrame$DayOfYear > 4 & epiFrame$FirstDayOfYear <= 3, 'EpiWeek'] <- epiFrame[is.na(epiFrame$EpiWeek) & epiFrame$DayOfYear > 4 & epiFrame$FirstDayOfYear <= 3, 'Week']
  epiFrame[is.na(epiFrame$EpiWeek) & epiFrame$DayOfYear <= 3 & epiFrame$DayOfWeek > 4 & epiFrame$FirstDayOfYear <= 4 & epiFrame$LastDayOfYear >= 4, 'EpiWeek'] <- 53
  epiFrame[is.na(epiFrame$EpiWeek) & epiFrame$DayOfYear <= 3 & epiFrame$DayOfWeek > 4 & epiFrame$FirstDayOfYear == 5, 'EpiWeek'] <- 53
  epiFrame[is.na(epiFrame$EpiWeek) & epiFrame$DayOfYear <= 3 & epiFrame$DayOfWeek >= 6 & epiFrame$FirstDayOfYear == 6 & epiFrame$LastDayOfYear == 6, 'EpiWeek'] <- 52
  epiFrame[is.na(epiFrame$EpiWeek) & epiFrame$DayOfYear <= 3 & epiFrame$DayOfWeek > 4 & epiFrame$FirstDayOfYear == 6 & epiFrame$LastDayOfYear == 6, 'EpiWeek'] <- 53
  epiFrame[is.na(epiFrame$EpiWeek) & epiFrame$DayOfYear <= 3 & epiFrame$DayOfWeek > 4, 'EpiWeek'] <- 52
  epiFrame[is.na(epiFrame$EpiWeek) & epiFrame$FirstDayOfYear >= 5, 'EpiWeek'] <- epiFrame[is.na(epiFrame$EpiWeek) & epiFrame$FirstDayOfYear >= 5, 'Week'] - 1
  epiFrame[is.na(epiFrame$EpiWeek), 'EpiWeek'] <- epiFrame[is.na(epiFrame$EpiWeek), 'Week']

  epiFrame[epiFrame$DayOfWeek == 7, 'Index'] <- 1
  epiFrame[!(is.na(epiFrame$Index)) & epiFrame$Index==1, 'newIndex'] <- seq(1, length(epiFrame[!(is.na(epiFrame$Index)) & epiFrame$Index==1, 'Index']), 1)
  epiFrame[,'Seq'] <- seq(1, length(epiFrame$Date), 1)

  epiFrame.top <- data.frame(epiFrame[epiFrame$Seq <= epiFrame[!(is.na(epiFrame$newIndex)) & epiFrame$newIndex==1, 'Seq'], ], AdjWeek = max(epiFrame[epiFrame$Seq <= epiFrame[!(is.na(epiFrame$newIndex)) & epiFrame$newIndex==1, 'Seq'], 'EpiWeek']), AdjYear = median(epiFrame[epiFrame$Seq <= epiFrame[!(is.na(epiFrame$newIndex)) & epiFrame$newIndex==1, 'Seq'], 'Year']))
  epiFrame.bot <- do.call(rbind, lapply(2:max(epiFrame$newIndex, na.rm=TRUE), function(x) data.frame(epiFrame[epiFrame$Seq <= epiFrame[!(is.na(epiFrame$newIndex)) & epiFrame$newIndex==x, 'Seq'] & epiFrame$Seq >= (epiFrame[!(is.na(epiFrame$newIndex)) & epiFrame$newIndex==x, 'Seq']-6), ], AdjWeek = rep(max(epiFrame[epiFrame$Seq <= epiFrame[!(is.na(epiFrame$newIndex)) & epiFrame$newIndex==x, 'Seq'] & epiFrame$Seq >= (epiFrame[!(is.na(epiFrame$newIndex)) & epiFrame$newIndex==x, 'Seq']-6), 'EpiWeek']), 7), AdjYear = rep(median(epiFrame[epiFrame$Seq <= epiFrame[!(is.na(epiFrame$newIndex)) & epiFrame$newIndex==x, 'Seq'] & epiFrame$Seq >= (epiFrame[!(is.na(epiFrame$newIndex)) & epiFrame$newIndex==x, 'Seq']-6), 'Year']), 7))))

  epiFrame <- rbind(epiFrame.top, epiFrame.bot)

  epiFrame <- epiFrame[,c('Date','AdjYear','AdjWeek')]
  colnames(epiFrame) <- c('Date','Year','Week')

  return(epiFrame)
}
