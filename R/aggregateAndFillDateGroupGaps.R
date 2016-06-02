#' Find the start date in the format of a character DateGroup using today's DateGroup as the starting point.
#'
#' The function sequences DateGroups from last date to today, incrementing by -1, and keeps the number of DateGroup units as specified by the user.
#'
#' @param msCalFrame is a data frame (non-sparse) of all dates and DateGroups, an output of \code{createCalendarLikeMicrosoft} function
#' @param yearSubGroup is a sub-group of a year, i.e. 'Week', 'Month', or 'Quarter'
#' @param gappedFrame is a data.frame containing a Year and [yearSubGroup], as well as additional columns that are sparesely populated
#' @param colsToAgg is a vector of character strings which specifies which variables should be used as arguments to {aggregate} the numerical column (functionCol)
#' @param startDate is a character string, such as what is returned from the \code{findStartDate} function
#' @param functionCol is acharacter string indicating which variable of the gappedFrame should be aggregated, the variable should contain numeric values
#' @param funToPerform is a character string specifying the function to pass to aggregate (options = 'sum','mean', etc.)
#' @param sparseHandling is a numeric parameter than indicates how to handle sparse entries (i.e. DateGroups without entries) with options NA, 0, or 1
#' @return a data frame that contains a DateGroup, colsToAgg, and a total of the functionCol for each DateGroup with the sparseHandling value for DateGroups not originally in the gappedFrame
#' @author Aimie Faucett
#' @details
#' The function takes in msCalFrame, yearSubGroup, gappedFrame, colsToAgg, startDate, functionCol, funToPerform, and sparseHandling parameter and
#' returns a data.frame that contains all DateGroups (per the msCalFrame), colsToAgg (e.g. 'Version', 'Key', 'RecordedValue'), functionCol
#' (e.g. 'Record' with sparseHandling filled into sparse dates for each unique combo of colsToAgg).
#' @export
#' @importFrom stats aggregate

aggregateAndFillDateGroupGaps <- function(msCalFrame, yearSubGroup, gappedFrame, colsToAgg, startDate, functionCol, funToPerform, sparseHandling) {

  baseFrame <- unique(msCalFrame[,c('Year', yearSubGroup, 'DateGroup')])
  baseFrame <- baseFrame[baseFrame[,'DateGroup'] >= startDate, ]
  gappedFrame[,'DateGroup'] <- ifelse(gappedFrame[,yearSubGroup] < 10,
                                      paste(gappedFrame[,'Year'], gappedFrame[,yearSubGroup], sep='-0'),
                                      paste(gappedFrame[,'Year'], gappedFrame[,yearSubGroup], sep='-'))

  formulaString <- paste(functionCol, paste('DateGroup', paste(colsToAgg, collapse='+'), sep='+'), sep='~')
  gappedFrame.agg <- with(gappedFrame[gappedFrame[,'DateGroup'] >= startDate, ], aggregate(as.formula(formulaString), FUN = funToPerform, na.rm = TRUE))

  gappedFrame.agg[,'combocat'] <- do.call(paste, c(gappedFrame.agg[,colsToAgg], sep=','))
  comboCats <- as.character(unique(gappedFrame.agg[,'combocat']))
  crossJoined <- do.call(rbind, lapply(1:length(comboCats), function(x) cbind(merge(unique(baseFrame[,c('Year','DateGroup')]), gappedFrame.agg[gappedFrame.agg[,'combocat'] == comboCats[x], c('DateGroup','Record')], all.x=TRUE, by='DateGroup'), combocat = comboCats[x])))

  decoded <- as.data.frame(sapply(1:3, function(x) do.call(rbind, strsplit(as.character(crossJoined[,'combocat']), split=','))[,x]))
  colnames(decoded) <- colsToAgg

  outFrame <- cbind(crossJoined[,c('DateGroup',functionCol)], decoded)
  outFrame <- outFrame[,c('DateGroup', colsToAgg, functionCol)]

  if(is.na(sparseHandling)) {

    return(outFrame)
  } else if(sparseHandling == 0) {

    outFrame[is.na(outFrame[,functionCol]), functionCol] <- 0
    return(outFrame)
  } else if(sparseHandling == 1) {

    outFrame[is.na(outFrame[,functionCol]), functionCol] <- 1
    return(outFrame)
  } else {

    stop('The sparseHandling parameter was not recognized. The parameter should be NA, 0, or 1.')
  }
}
