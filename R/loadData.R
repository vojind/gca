#ALL THESE ARE USED ONLY TO MAKE A DATAFRAME FROM FILES
#THIS HAS BEEN USED FOR MAKING THE EXAMPLE DATA
#' @import dplyr
NULL
#' @import magrittr
NULL

#' @title split by gender
#' @description splits data frames by gender
#' @param tibble Takes in tibble subject to splitting
#' @return returns list with two dataframes, one for each gender
splitByGender <- function(tibble){
  df <- as.data.frame(tibble)
  half <- nrow(df)/2
  dflist <- list("M" = df[1:half,-1], "W" = df[(half+1):(2*half),-1])
  return(dflist)
}

#' @title Melt data
#' @description melt two dimensional array into dataframe, each entry gets a unique row
#' @param df two dimensional array
#' @param type incidence or mortality data
meltAndClean <- function(df,type){
  curr <- as.data.frame(df)
  rownames(curr) <- unique(df[[1]])
  curr <- reshape2::melt(t(curr[,-1]),
               value.name = type,
               varnames = c("period", "agegroup"))
  curr[is.na(curr)] <- 0
  return(curr)
}

#' @title Collapse Age groups
#' @description collapse age groups in population and incidence data,
#' as mortality data often has 0-14 yrs as one single group
#' @param df dataframe
collapseAgeGroups <- function(df){
  df$agegroup=gsub("0 - 4|5 - 9|10 - 14","0 - 14",df$agegroup)
  df$agegroup=gsub("40 - 144","40 - 44",df$agegroup)
  f <- stats::as.formula(sprintf("%s ~ %s + %s", names(df)[3],"period","agegroup"))
  return(stats::aggregate(f,df,sum))
}

#' @title merge data
#' @description merge data for incidence, population and mortality for one specific federal state
#' @param inc list with incidence data frame
#' @param pop list with population data frame
#' @param mort list with mortality data frame
#' @param sheet abbrevation specifying federal state
mergeData <- function(inc, pop, mort, sheet){
  if(length(inc)>1){
    men <-
      merge(x = inc$M, y = pop$M, by = c("period","agegroup"), all.x = TRUE)%>%
      merge(x=.,y=mort$M, by = c("period","agegroup"), all.x = TRUE)
    men['FedState'] <- sheet
    women <-
      merge(x = inc$W, y = pop$W, by = c("period","agegroup"), all.x = TRUE)%>%
      merge(x=.,y=mort$W, by = c("period","agegroup"), all.x = TRUE)
    women['FedState'] <- sheet
    return(list("M" = men,"W" = women))
  }
  else{
    half <- length(pop[[1]])/2
    pop[[1]]$population <- (pop[[1]]$population + pop[[2]]$population)
    pop[[2]] <- NULL
    both <-
      merge(x = inc[[1]], y = pop[[1]], by = c("period","agegroup"), all.x = TRUE)%>%
      merge(x=.,y=mort[[1]], by = c("period","agegroup"), all.x = TRUE)
    both['FedState'] <- sheet
    return(list("B" = both))
  }
}

#' @title read sheet
#' @description read sheet for one Federal state
#' @param name name of file
#' @param type specifying whether its incidence, mortality or population data
#' @param sheet sheet abbrevation specifying federal state, for example 'BB'
read_sheet <- function(name, type, sheet){
  tibble <- suppressMessages(readxl::read_excel(name, sheet=sheet, skip = 2, col_names = T))
  gender <- tibble[1,1]=="M"
  dflist <- if(gender) splitByGender(tibble) else list(tibble)
  dflist <- lapply(dflist, function(x) meltAndClean(x,type))
  return(dflist)
}

#' @title fix data for single federal state
#' @description gets incidence, population and mortality data for singel federal state
#' and mereges together using mergeData
#' @param incFile incidence file name
#' @param popFile population file name
#' @param mortFile mortality file name
#' @param sheet abbrevation for federal state, specifying excel sheet
fed_total <- function(incFile, popFile, mortFile, sheet){
  inc <-  read_sheet(incFile, "incidence", sheet)
  pop <-  read_sheet(popFile, "population", sheet)
  mort <- read_sheet(mortFile, "mortality", sheet)
  if('0 - 14' %in% mort[[1]]$agegroup){
    inc <- lapply(inc, collapseAgeGroups)
    pop <- lapply(pop, collapseAgeGroups)
  }
  mergeData(inc,pop,mort,sheet)
}




#' @title check data available/complete
#' @description using the registry, check which rows are available/complete
#' @param df the dataframe to be checked
#' @param registry the registry dataframe
checkValidity <- function(df, registry){
  #id <- which(colnames(df) == 'period')
  id = 6
  available <- apply(X = df,
                     MARGIN = 1,
                     function(x) dplyr::between(x[1], registry[x[id],'AvailFrom'], registry[x[id],'AvailTo']))
  complete <- apply(X = df,
                    MARGIN = 1,
                    function(x) dplyr::between(x[1], registry[x[id],'CheckedFrom'], registry[x[id],'CheckedTo']))
  return(cbind(df,
               available,
               complete))
}

#' @title clean and combine files
#' @description cleans and combines all three files, combining them to data frames and checks validity
#' @param incFile name of incidence file
#' @param popFile name of population file
#' @param mortFile name of mortality file
#' @return a list containing all three data frames
#' @export
cleanAndCombine <- function(incFile,popFile,mortFile){
  test <- suppressMessages(readxl::read_excel(incFile, sheet=2, skip = 2, col_names = T))
  gender <- if(test[1,1]=="M") T else F
  sheets <- readxl::excel_sheets(incFile)[-1]
  currList <- if(gender) list("M" = data.frame(),"W" = data.frame()) else list("B" = data.frame())
  for(i in 1:length(currList)){
    curr <- currList[[i]]
    for(sheet in sheets){
      curr <- rbind(curr,fed_total(incFile,popFile,mortFile,sheet)[[i]])
    }
    curr %<>% mutate_if(is.numeric,as.integer)
    curr %<>% mutate_if(is.character,as.factor)
    #currList[[i]] <- checkValidity(curr, readReg())
    currList[[i]] <- checkValidity(curr, registry)
  }
  return(currList)
}

#NOT USED NOW
if(F){
  readReg <- function(name="../gca_package/Trondheim/Uebersicht_Register_.xlsx"){
    registry <- as.data.frame(read_excel(name, sheet=1, skip=2,
                                         col_names = c("name", "FedState", "AvailFrom",
                                                       "AvailTo", "CheckedFrom", "CheckedTo"),n_max = 17))
    i <- c(3, 4, 5, 6)
    registry[ , i] <- apply(registry[ , i], 2, function(x) as.integer(as.character(x)))
    registry %<>% mutate_if(is.character,as.factor)
    rownames(registry) <- registry$FedState
    return(subset(registry, select = -FedState))
  }
  load_file <- function(name, path) {
    ext <- tools::file_ext(name)
    switch(ext,
           csv = vroom::vroom(path, delim = ",", col_types = list()),
           tsv = vroom::vroom(path, delim = "\t", col_types = list()),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  }
}#I beliece this is redundant



