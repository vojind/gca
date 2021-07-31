#' @title european standard weights
#' @description european standard weight for age standardized analysis
weights <- list(
  "0 - 14" = 0.16,
  "15 - 19" = 0.055,
  "20 - 24" = 0.06,
  "25 - 29" = 0.06,
  "30 - 34" = 0.065,
  "35 - 39" = 0.7,
  "40 - 44" = 0.7,
  "45 - 49" = 0.7,
  "50 - 54" = 0.7,
  "55 - 59" = 0.065,
  "60 - 64" = 0.06,
  "65 - 69" = 0.055,
  "70 - 74" = 0.05,
  "75 - 79" = 0.04,
  "80 - 84" = 0.025,
  "85 plus" = 0.025
  )

#' @title all states
#' @description All 16 federal states of Germany
states <<- c("ALL","BB","BE","BW","BY","HB","HE","HH","MV",
  "NI","NW","RP","SH","SL","SN","ST","TH")

#' @title complete states
#' @description when aggregating data over states, only these states are used.
#' These have complete data from 2001 to 2014.
completeStates <<- c("SH","HH","HB","SL","BB","MV","SN","TH")

#' @title Removing NaN values
#' @description Nans in the complete/available columns are set to FALSE.
#' @param df The data frame whose Nan values are set to False
#' @return The dataframe
#' @export

removeNans <- function(df){
  nans <- is.na(df$complete)
  df$complete[nans] <- F
  return(df)
}

#' @title prepareData
#' @description prepares data by choosing fedState or summing over fedState. Also calculates values per capita
#' @param df dataframe to prepare
#' @param fstate federalstate, defaults to ALL*
#' @return cleaned dataframe
#' @export

#agegroup analysis ***
prepareData <- function(df, fstate = "ALL*"){
  if(!(fstate %in% c("ALL*", "ALL"))){
    df <- subset(df, FedState == fstate)
    df$incRate <- with(df, incidence*(1e5/population)*as.numeric(weights[as.character(agegroup)]))
    df$mortRate <- with(df, mortality*(1e5/population)*as.numeric(weights[as.character(agegroup)]))
  }
  else{
    #_-----------------------------___#
    df <- subset(df, FedState %in% completeStates)
    df <- dplyr::summarise(dplyr::group_by(df,
                                           agegroup = agegroup,
                                           period = period),
                           incidence = sum(incidence),
                           mortality = sum(mortality),
                           population= sum(population))
    df$incRate <- with(df, incidence*(1e5/population)*as.numeric(weights[as.character(agegroup)]))
    df$mortRate <- with(df, mortality*(1e5/population)*as.numeric(weights[as.character(agegroup)]))
  }
  return(df)
}

#' @title complete years for federal state
#' @description get years for which the specified federal state have complete data
#' @param fstate federal state, defaults to 'ALL*'
#' @return two integers, indicating start and end of validated/complete period
getCompleteYears <- function(fstate){
  years <-
    if(fstate %in% c('ALL*',"ALL")) c(2001,2014) else registry[fstate, c('CheckedFrom', 'CheckedTo')]
  return(c(years[[1]], years[[2]]))
}
