#' @title plot age group plot
#' @description figure with separate plot for each data frame, plotting incidence or mortality against period
#' @param df dataframe
#' @param fstate federal state
#' @export
plotAgGrid <- function(df, fstate){
  colors <- c("incidence" = "blue", "mortality" = "red", "complete" = "black")
  ggplot(df, aes(x=period)) +
    geom_line(aes(y=incPer100k, color = "incidence"), size = 1, alpha = 0.7) +
    geom_line(aes(y=mortPer100k, color= "mortality"), size = 1, alpha= 0.7) +
    facet_wrap(~agegroup) +
    theme_bw(legend.title = element_blank()) +
    ylab('Rate per 100,000 persons') +
    if(fstate != "ALL") geom_vline(xintercept=getCompleteYears(fstate), linetype=2)
}

#' @title plot age group plot
#' @description figure with separate plot for each data frame, plotting mortality divided by incidence against period
#' @param df dataframe
#' @param fstate federal state
#' @export
plotAgMortPerInc <- function(df, fstate){
  ggplot(df, aes(x=period)) +
    geom_line(aes(y=mortPer100k/incPer100k, color = "mortality/incidence"), size = 1, alpha = 0.7) +
    facet_wrap(~agegroup) +
    theme_bw(legend.title = element_blank()) +
    ylab('Rate per 100,000 persons') +
    if(fstate != "ALL") geom_vline(xintercept=getCompleteYears(fstate), linetype=2)
}
###---------------------------------MODULE----------------------------
AgeGroupPlotUI <- function(id) {
  states <-
    c("ALL","BB","BE","BW","BY","HB","HE","HH","MV",
      "NI","NW","RP","SH","SL","SN","ST","TH")
  sidebarLayout(
    sidebarPanel(selectInput(NS(id,"state"),
                             "Select a state",
                             choices=states)),
    mainPanel(plotOutput(NS(id,"plot")),
              plotOutput(NS(id,"plot2")))
  )
}

AgeGroupPlotServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    agDf <- reactive(prepareData(data(),input$state))
    output$plot <- renderPlot(plotAgGrid(agDf(),input$state))
    output$plot2 <- renderPlot(plotAgMortPerInc(agDf(),input$state))
  })
}

