#' @title Removing NaN values
#' @description Nans in the complete/available columns are set to FALSE.
#' @param df The data frame whose Nan values are set to False
#' @return The dataframe
#' @export

removeNans <- function(df){
  nans <- is.na(df$complete)
  df$complete[nans] <- F
  testdata <<- df
  return(df)
}

#' @title prepareData
#' @description prepares data by choosing fedState or summing over fedState. Also calculates values per capita
#' @param df dataframe to prepare
#' @param fstate federalstate, defaults to ALL
#' @return cleaned dataframe
#' @export

prepareData <- function(df, fstate = "ALL"){
  if(fstate != "ALL"){
    df <- subset(df, FedState == fstate)
  }
  else{
    df <- dplyr::summarise(dplyr::group_by(df,
                             agegroup = agegroup,
                             period = period),
                    incidence = sum(incidence),
                    mortality = sum(mortality),
                    population= sum(population))
  }
  df$incPer100k  <- df$incidence/df$population*1e5
  df$mortPer100k <- df$mortality/df$population*1e5
  return(df)
}

#' @title complete years for federal state
#' @description get years for which the specified federal state have complete data
#' @param fstate federal state, defaults to 'ALL'
#' @return two integers, indicating start and end of validated/complete period
getCompleteYears <- function(fstate){
  years <-
    if(fstate=='ALL') c(2001,2014) else registry[fstate, c('CheckedFrom', 'CheckedTo')]
  return(c(years[[1]], years[[2]]))
}
