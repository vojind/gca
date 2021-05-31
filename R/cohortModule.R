
#' @title convert agegroups
#' @description convert agegroups to median integer
#' @param string age group string to be converted to an int

convertInterval <- function(string){
  string <- as.character(string)
  chars <- strsplit(string,' ')[[1]]
  num <- strtoi(chars[1]) + strtoi(chars[length(chars)])
  if(is.na(num)){
    temp <- unname(sapply(chars, strtoi))
    return(temp[!is.na(temp)])
  }
  else return(as.integer(num/2))
}

#' @title prepare data for cohort analysis
#' @description data preparation similar to PrepareData(), but with a few extra steps
#' @param data dataframe
#' @param fstate abbrevation for federal state
tweakCohortData <- function(data, fstate){
  years <- getCompleteYears(fstate)
  df <- prepareData(data, fstate)
  df <- subset(df, years[1] <= period & period <= years[2])
  #df <- subset(df, FedState %in% c("SH","HH","HB","SL","BB","MV","SN","TH"))
  df$age           <- as.factor(sapply(df$agegroup, FUN=convertInterval))
  df$birthYear     <- df$period - as.integer(as.character(df$age))
  return(df)}

#' @title cohort plots - incidence
#' @description plot cohort plots for incidence data
#' @param df dataframe
#' @export
plotCohortInc <- function(df){
  one <-
    ggplot(df, aes(birthYear, y=incPer100k, colour=age)) +
    geom_line() +
    theme_bw() +
    ylab('Incidence rate per 100,000 persons') +
    xlab('Year of Birth') +
    directlabels::geom_dl(aes(label = age), method = list(directlabels::dl.trans(x = x + 0.2), "last.points", cex = 0.8))
  two <-
    ggplot(df, aes(period, y=incPer100k, colour=age)) +
    geom_line() +
    theme_bw() +
    ylab('Incidence rate per 100,000 persons')+
    xlab('Year of Death') +
    directlabels::geom_dl(aes(label = age), method = list(directlabels::dl.trans(x = x + 0.2), "last.points", cex = 0.8))
  gridExtra::grid.arrange(one, two, ncol=2)}

#' @title cohort plots - mortality
#' @description plot cohort plots for mortality
#' @param df dataframe
#' @export
plotCohortMort <- function(df){
  three <-
    ggplot(df, aes(birthYear, y=mortPer100k, colour=age)) +
    geom_line() +
    theme_bw() +
    ylab('Mortality rate per 100,000 persons') +
    xlab('Year of Birth')+
    directlabels::geom_dl(aes(label = age), method = list(directlabels::dl.trans(x = x + 0.2), "last.points", cex = 0.8))

  four <-
    ggplot(df, aes(period, y=mortPer100k, colour=age)) +
    geom_line() +
    theme_bw() +
    ylab('Mortality per 100,000 persons')+
    xlab('Year of Death')+
    directlabels::geom_dl(aes(label = age), method = list(directlabels::dl.trans(x = x + 0.2), "last.points", cex = 0.8))

  gridExtra::grid.arrange(three, four, ncol=2)
}

###--------------module functions--------------######

cohortPlotUI <- function(id) {
  states <-
    c("ALL","BB","BE","BW","BY","HB","HE","HH","MV",
      "NI","NW","RP","SH","SL","SN","ST","TH")
  fluidPage(
    selectInput(NS(id,"state"),
                "Select a state",
                choices=states),
    plotOutput(NS(id,"inc")),
    plotOutput(NS(id,"mort"))
  )
}


cohortPlotServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    df <- reactive(
      tweakCohortData(data(),input$state))
    output$inc <- renderPlot(plotCohortInc(df()))
    output$mort <- renderPlot(plotCohortMort(df()))
  })
}
