#' Merge data frames with date-sparse data
#'
#' This function merges two data frames that are created using aggregateAndFillDataGroupGaps and creates a rate with roll capability and sparse entry handling.
#'
#' @param numFrame a sparse data frame created using the \code{aggregateAndFillDateGroupGaps} function
#' @param denomFrame a sparse data frame created using the \code{aggregateAndFillDateGroupGaps} function
#' @param mergeColsX a vector of character strings specifying which columns to use in the \code{\link[base]{merge}} function's by.x parameter
#' @param mergeColsY a vector of character strings specifying which columns to use in the \code{\link[base]{merge}} function's by.y parameter
#' @param numNumeric a character string that identifies which column of the numFrame contains the numeric value to use as the numerator in the rate calculation
#' @param denomNumeric a character string that identifies which column of the denomFrame contains the numeric value to use as the denominator in the rate calculation
#' @param sparseHandling a numeric (either NA or 0) that tells the function how to handle sparse Rate values after the merge
#' @param rollPeriods a numeric (default = 0) that indicates the number of periods to include in the rolling rate (0, default, is not rolled)
#' @return a data frame that contains the orginal columns in numFrame as well as a Rate that is calculated using the numNumeric/denomNumeric and adjusted using sparseHandling for DateGroups that are sparse in the numFrame
#' @author Aimie Faucett
#' @details
#' This function takes a numFrame, denomFrame, mergeColsX, mergeColsY, numNumeric, denomNumeric, sparseHandling, rollPeriods, and lagPeriods. It
#' returns a data.frame that contains a DateGroup, columns from numFrame (less the numNumeric column), and a Rate (either rolled or not with
#' sparse entries handled as specified by the sparseHandling parameter). The rate is calculated by merging the numFrame and denomFrame by the
#' mergeColsX and mergeColsY and then finding a rate for each unique combination of the merged columns.
#' @export

mergeCalSparseFrames <- function(numFrame, denomFrame, mergeColsX, mergeColsY, numNumeric, denomNumeric, sparseHandling, rollPeriods = 0) {

  colnames(numFrame)[grep(paste('\\b',numNumeric,'\\b',sep=''), colnames(numFrame))] <- 'numRecord'
  colnames(denomFrame)[grep(paste('\\b',denomNumeric,'\\b',sep=''), colnames(denomFrame))] <- 'denomRecord'

  mrgFrame <- merge(numFrame, denomFrame[,c(mergeColsY, 'denomRecord')], by.x = mergeColsX, by.y = mergeColsY)

  if(rollPeriods > 0) {

    colsToAgg <- colnames(mrgFrame)[!(colnames(mrgFrame) %in% c('DateGroup','numRecord','denomRecord'))]

    if(length(colsToAgg) <= 1) {

      mrgFrame[,'combocat'] <- mrgFrame[,colsToAgg]
    } else {

      mrgFrame[,'combocat'] <- do.call(paste, c(mrgFrame[,colsToAgg], sep=','))
    }

    comboCats <- as.character(unique(mrgFrame[,'combocat']))
    numRoll <- do.call(rbind, lapply(1:length(comboCats), function(x) cbind(DateGroup = as.character(unique(mrgFrame[,'DateGroup']))[rollPeriods:length(as.character(unique(mrgFrame[,'DateGroup'])))], combocat = comboCats[x], sapply(rollPeriods:length(as.character(unique(mrgFrame[mrgFrame[,'combocat'] == comboCats[x], 'DateGroup']))), function(y) ifelse(sum(is.na(mrgFrame[mrgFrame[,'combocat'] == comboCats[x], 'numRecord'][(y-(rollPeriods-1)):y])) == rollPeriods, NA, sum(mrgFrame[mrgFrame[,'combocat'] == comboCats[x], 'numRecord'][(y-(rollPeriods-1)):y], na.rm=TRUE))))))
    denomRoll <- do.call(rbind, lapply(1:length(comboCats), function(x) cbind(DateGroup = as.character(unique(mrgFrame[,'DateGroup']))[rollPeriods:length(as.character(unique(mrgFrame[,'DateGroup'])))], combocat = comboCats[x], sapply(rollPeriods:length(as.character(unique(mrgFrame[mrgFrame[,'combocat'] == comboCats[x], 'DateGroup']))), function(y) ifelse(sum(is.na(mrgFrame[mrgFrame[,'combocat'] == comboCats[x], 'denomRecord'][(y-(rollPeriods-1)):y])) == rollPeriods, NA, sum(mrgFrame[mrgFrame[,'combocat'] == comboCats[x], 'denomRecord'][(y-(rollPeriods-1)):y], na.rm=TRUE))))))
    rolled <- merge(numRoll, denomRoll, by=c('DateGroup','combocat'))
    mrgFrame <- merge(mrgFrame, rolled, by=c('DateGroup','combocat'))
    colnames(mrgFrame)[grep('V3', colnames(mrgFrame))] <- c('numRoll','denomRoll')
    mrgFrame[,'numRoll'] <- as.numeric(as.character(mrgFrame[,'numRoll']))
    mrgFrame[,'denomRoll'] <- as.numeric(as.character(mrgFrame[,'denomRoll']))
    mrgFrame[,'Rate'] <- with(mrgFrame, numRoll/denomRoll)

  } else if(rollPeriods == 0) {

    mrgFrame[,'Rate'] <- with(mrgFrame, numRecord/denomRecord)
  }else {

    stop('The rollPeriods parameter entered is of the wrong class. The parameter must be a numeric integer.')
  }

  if(is.na(sparseHandling)) {

    mrgFrame[is.nan(mrgFrame[,'Rate']),'Rate'] <- NA
    mrgFrame[is.infinite(mrgFrame[,'Rate']),'Rate'] <- NA
  } else if(sparseHandling == 0) {

    mrgFrame[is.na(mrgFrame[,'Rate']),'Rate'] <- 0
    mrgFrame[is.nan(mrgFrame[,'Rate']),'Rate'] <- 0
    mrgFrame[is.infinite(mrgFrame[,'Rate']),'Rate'] <- 0

  } else {

    stop('The sparseHandling parameter is not specified correctly. It must be NA or 0.')
  }

  trimCols <- colnames(mrgFrame)[grep('combocat|num|denom', colnames(mrgFrame))]
  mrgFrame <- mrgFrame[,!(colnames(mrgFrame) %in% trimCols)]
  return(mrgFrame)
}
