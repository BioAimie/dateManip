#' Return the mean and limits for the sparse data frame, allowing for alternate limits and one sided or two sided limits.
#'
#' This function takes a data frame outputted from the mergeCalSparseFrames function and paritions the data frame by the rateParitionVec parameter. It then finds the mean and limits, and colors data points accordingly.
#'
#' @param sparseRateFrame a sparse data frame created using the \code{mergeCalSparseFrames} function
#' @param ratePartitionVec a character string vector that specifies how to parition the categories in the data frame
#' @param ignorePeriods a numeric, default of 0, that indicates how many periods from the current DateGroup to neglect in calculating the average and standard deviation
#' @param returnLimits a logical with default FALSE that specifies whether or not the data frame returned should have limits
#' @param limitFactor a numeric, default of 3, that specifies the number of standard deviations from the mean to set the limit
#' @param limitSide a character string, default of upper, that indicates whether the limit should be +X from the mean (upper), -X from the mean (lower), or +/-X from the mean (two.sided)
#' @param altUL a numeric, default of 0.001, that indicates a default limit if the altUL > limitFactor*sd(X)
#' @param altLL a numeric, default of 0, that indicates a default limit if the altLL < limitFactor*sd(X)
#' @param dategroup is a string indicating which dategroup the input sparseRateFrame has, i.e. "Week", "Month", "Quarter"
#' @param keepPeriods is an integer indicating the number of dategroup periods that will going into calculating the mean/standard deviation (i.e. 2 would mean 2 weeks if you have week dategroups)
#' @return a data frame that contains the orginal columns in sparseRateFrame, as well as an average for each of the ratePartitionVec combinations. It may also have limits if limit parameters are specified.
#' @author Aimie Faucett
#' @details
#' This function takes in a sparseRateFrame that is made using the \code{mergeCalSparseFrames} function and return the mean for each unique combination of
#' the entries in the ratePartitionVec. If the limit parameters are entered, the Rate variable is colored as 'pass' or 'review' based on limitFactor*sd(X)
#' and that value is either added, subtracted, or added and subtracted to make a range about the mean where the data set falls in or out of teh limits. The
#' altUL and altLL allow the user to specify an alternate limit that can be applied instead of the limitFactor.
#' @export

addStatsToSparseHandledData <- function(sparseRateFrame, ratePartitionVec, ignorePeriods = 0, returnLimits = FALSE, limitFactor = 3, limitSide = 'upper', altUL = 0.001, altLL = 0.00, keepPeriods) {

  if(length(ratePartitionVec) == 1) {
    sparseRateFrame[,'combocat'] <- sparseRateFrame[,ratePartitionVec]
  } else {
    sparseRateFrame[,'combocat'] <- do.call(paste, c(sparseRateFrame[,ratePartitionVec], sep=','))
  }

  comboCats <- as.character(unique(sparseRateFrame[,'combocat']))

  if(keepPeriods == 0){
    avgFrame <- as.data.frame(do.call(rbind, lapply(1:length(comboCats), function(x) cbind(combocat = comboCats[x], Avg = mean(sparseRateFrame[sparseRateFrame[,'combocat'] == comboCats[x], ][with(sparseRateFrame[sparseRateFrame[,'combocat'] == comboCats[x], ], order(DateGroup)), 'Rate'][1:(length(sparseRateFrame[sparseRateFrame[,'combocat'] == comboCats[x],'DateGroup']) - ignorePeriods)], na.rm=TRUE)))))
    avgFrame[,'Avg'] <- as.numeric(as.character(avgFrame[,'Avg']))
    avgFrame[is.nan(avgFrame[,'Avg']),'Avg'] <- NA
    sparseRateFrame <- merge(sparseRateFrame, avgFrame, by='combocat')

  }else{
    comboCat.list <- vector(mode="list")
    for(x in comboCats) {

      combocat.df <- sparseRateFrame[which(sparseRateFrame$combocat == x), ]
      combocat.df <- combocat.df[order(combocat.df$DateGroup), ]

      unique.dates <- unique(combocat.df$DateGroup)
      start.index <- min(which(combocat.df$DateGroup == unique.dates[keepPeriods+1] ))

      averages <- unlist(lapply(combocat.df$DateGroup[start.index:nrow(combocat.df)], function(j)mean(combocat.df[which(combocat.df$DateGroup %in% unique.dates[(which(unique.dates == j)-keepPeriods):(which(unique.dates == j)-ignorePeriods)]), 'Rate'], na.rm=TRUE)))
      averagecat <-  paste( combocat.df$DateGroup[start.index:nrow(combocat.df)] , ",", x, sep="")
      comboCat.list[[x]] <- cbind(dateGroupcombocat=averagecat, Avg=averages)
    }
    avgFrame <- as.data.frame(do.call(rbind, comboCat.list))
    avgFrame[,'Avg'] <- as.numeric(as.character(avgFrame[,'Avg']))
    avgFrame[is.nan(avgFrame[,'Avg']),'Avg'] <- NA
    sparseRateFrame$dateGroupcombocat <- paste(sparseRateFrame$DateGroup, ",", sparseRateFrame$combocat, sep="")
    sparseRateFrame.original <- sparseRateFrame
    sparseRateFrame <- unique(merge(sparseRateFrame, avgFrame, by='dateGroupcombocat'))
  }

  if(returnLimits == FALSE) {

    sparseRateFrame <- sparseRateFrame[!(is.na(sparseRateFrame[,'Avg'])), ]
    keepCols <- colnames(sparseRateFrame)[!(colnames(sparseRateFrame) %in% colnames(sparseRateFrame)[grep('combocat', colnames(sparseRateFrame))])]
    return(sparseRateFrame[,keepCols])
  } else {
    if(keepPeriods == 0){
      sdFrame <- as.data.frame(do.call(rbind, lapply(1:length(comboCats), function(x) cbind(combocat = comboCats[x], Sdev = sd(sparseRateFrame[sparseRateFrame[,'combocat'] == comboCats[x], ][with(sparseRateFrame[sparseRateFrame[,'combocat'] == comboCats[x], ], order(DateGroup)), 'Rate'][1:(length(sparseRateFrame[sparseRateFrame[,'combocat'] == comboCats[x],'Rate']) - ignorePeriods)], na.rm=TRUE)))))
      sparseRateFrame <- merge(sparseRateFrame, sdFrame, by='combocat')
    } else {
      comboCat.list <- vector(mode="list")
      for(x in comboCats){

        combocat.df <- sparseRateFrame.original[which(sparseRateFrame.original$combocat == x), ]
        combocat.df <- combocat.df[order(combocat.df$DateGroup), ]

        unique.dates <- unique(combocat.df$DateGroup)
        start.index <- min(which(combocat.df$DateGroup == unique.dates[keepPeriods+1] ))

        sdevs <- unlist(lapply(combocat.df$DateGroup[start.index:nrow(combocat.df)], function(j)sd(combocat.df[which(combocat.df$DateGroup %in% unique.dates[(which(unique.dates == j)-keepPeriods):(which(unique.dates == j)-ignorePeriods)]), 'Rate'], na.rm=TRUE)))
        sdevcat <-  paste( combocat.df$DateGroup[start.index:nrow(combocat.df)] , ",", x, sep="")
        comboCat.list[[x]] <- cbind(dateGroupcombocat=sdevcat, Sdev=sdevs)
      }

      sdFrame <- as.data.frame(do.call(rbind, comboCat.list))
      sparseRateFrame$dateGroupcombocat <- paste(sparseRateFrame$DateGroup, ",", sparseRateFrame$combocat, sep="")
      sparseRateFrame <- unique(merge(sparseRateFrame, sdFrame, by='dateGroupcombocat'))
    }
    sparseRateFrame[,'Sdev'] <- as.numeric(as.character(sparseRateFrame[,'Sdev']))

    if(limitSide == 'upper') {

      sparseRateFrame[,'UL'] <- sparseRateFrame[,'Avg'] + limitFactor*sparseRateFrame[,'Sdev']
      sparseRateFrame[,'UL'] <- with(sparseRateFrame, ifelse(UL < altUL, altUL, UL))
      sparseRateFrame[,'Color'] <- with(sparseRateFrame, ifelse(Rate > UL, 'review','pass'))
    } else if(limitSide == 'lower') {

      sparseRateFrame[,'LL'] <- sparseRateFrame[,'Avg'] - limitFactor*sparseRateFrame[,'Sdev']
      sparseRateFrame[,'LL'] <- with(sparseRateFrame, ifelse(LL > altLL, altLL, LL))
      sparseRateFrame[,'Color'] <- with(sparseRateFrame, ifelse(Rate < LL, 'review','pass'))
    } else if(limitSide == 'two.sided') {

      sparseRateFrame[,'UL'] <- sparseRateFrame[,'Avg'] + limitFactor*sparseRateFrame[,'Sdev']
      sparseRateFrame[,'LL'] <- sparseRateFrame[,'Avg'] - limitFactor*sparseRateFrame[,'Sdev']
      sparseRateFrame[,'UL'] <- with(sparseRateFrame, ifelse(UL < altUL, altUL, UL))
      sparseRateFrame[,'LL'] <- with(sparseRateFrame, ifelse(LL > altLL, altLL, LL))
      sparseRateFrame[,'Color'] <- with(sparseRateFrame, ifelse(Rate < LL | Rate > UL, 'review','pass'))
    } else {

      stop("The limitSide parameter has been specified incorrectly... the function take three options: 'upper', 'lower', or 'two.sided'.")
    }

    keepCols <- colnames(sparseRateFrame)[!(colnames(sparseRateFrame) %in% colnames(sparseRateFrame)[grep('combocat', colnames(sparseRateFrame))])]
    return(sparseRateFrame[,keepCols])
  }
}
