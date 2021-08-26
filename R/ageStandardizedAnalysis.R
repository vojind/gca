

#' @title Data preparation II
#' @description Prepares data for age standardized analysis.
#' Resembles prepareData(). Final dataframe only has period, incRate and mortRate
#' @param data data frame for preparation
#' @param fstate specifying federal state
#' @return prepared data frame, ready for further analyses
#' @examples
#' prostate <- cancerData[['prostate']][[1]]
#' data <- tweakAsisData(prostate,'SL')
#' plotAsis(data,'SL')
tweakAsisData <- function(data, fstate){
  years <- getCompleteYears(fstate)
  df <- prepareData(data, fstate)
  df <- as.data.frame(dplyr::summarise(group_by(df,
                                                period = period),
                                       incRate = sum(incRate),
                                       mortRate = sum(mortRate)))
  return(df)}



#' @title plot age standardized analysis
#' @description plots age standardized analysis
#' @param df dataframe, prepared using tweakAsisData
#' @param fstate specifying federal state
#' @examples
#' prostate <- cancerData[['prostate']][[1]]
#' data <- tweakAsisData(prostate,'SL')
#' plotAsis(data,'SL')
plotAsis <- function(df,fstate){
  ggplot(df, aes(x=period)) +
    geom_line(aes(y=incRate, colour='incidence rate')) +
    geom_line(aes(y=mortRate, colour='mortality rate')) +
    theme_bw() +
    theme(legend.title = element_blank()) +
    theme(text = element_text(size = 15))+
    ylab('Age standardized rates per 100,000 persons') +
    xlab('period') +
    geom_vline(xintercept=getCompleteYears(fstate), linetype=2)
}


#----------------------------MODULE--------------------------------------

asisPlotUI <- function(id) {
  states <-
    c("ALL","BB","BE","BW","BY","HB","HE","HH","MV",
      "NI","NW","RP","SH","SL","SN","ST","TH")
  fluidPage(
    fluidRow(
      column(width=8,
             selectInput(NS(id,"state"),
                         "Select a state",
                         choices=states)),
      column(width=4, align='right',
             htmlOutput(NS(id,"hei")))),
    fluidRow(downloadButton(NS(id, 'downloadPlot'),'Download Plot')),
    plotOutput(NS(id,"plot")),

  )
}


asisPlotServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$hei<- renderText(paste('<B>data:</B> ',choice()))
    df <- reactive(
      tweakAsisData(data(),input$state))
    plotVar <- reactive(plotAsis(df(),input$state))
    output$plot <- renderPlot(plotVar())
    output$downloadPlot <- downloadHandler(
      filename = function(){'asis.pdf'},
      content = function(file){ggsave(file, plot=plotVar(), width=12, height=6, units = "in")}
    )
  })
}
install.packages("BAPC", repos = "http://R-Forge.R-project.org")

