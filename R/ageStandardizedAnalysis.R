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

#' @title Prepare data for age standardized analysis
#' @description Prepares data for age standardized analysis.
#' Pretty similar to prepareData(), except for a few things.
#' Makes two new columns for the reweigheted incidence and mortality rates
#' @param data data frame for preparation
#' @param fstate specifying federal state
#' @return prepared data frame, ready for age standardized analysis

tweakAsisData <- function(data, fstate){
  years <- getCompleteYears(fstate)
  df <- prepareData(data, fstate)
  df <- subset(df, years[1] <= period & period <= years[2])
  df$newIncRate <- with(df, incPer100k*as.numeric(weights[as.character(agegroup)]))
  df$newMortRate <- with(df, mortPer100k*as.numeric(weights[as.character(agegroup)]))
  df <- as.data.frame(dplyr::summarise(group_by(df,
                                   period = period),
                          newIncRate = sum(newIncRate),
                          newMortRate = sum(newMortRate)))
  return(df)}


#' @title plot age standardized analysis
#' @description plots age standardized analysis
#' @param df dataframe
plotAsis <- function(df){
  ggplot(df, aes(x=period)) +
    geom_line(aes(y=newIncRate, colour='incidence rate')) +
    geom_line(aes(y=newMortRate, colour='mortality rate')) +
    theme_bw() +
    ylab('Age standardized rates per 100,000 persons') +
    xlab('period')
}


#----------------------------MODULE--------------------------------------

asisPlotUI <- function(id) {
  states <-
    c("ALL","BB","BE","BW","BY","HB","HE","HH","MV",
      "NI","NW","RP","SH","SL","SN","ST","TH")
  fluidPage(
    selectInput(NS(id,"state"),
                "Select a state",
                choices=states),
    plotOutput(NS(id,"inc")),
  )
}


asisPlotServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    df <- reactive(
      tweakAsisData(data(),input$state))
    output$inc <- renderPlot(plotAsis(df()))
  })
}
