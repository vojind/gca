#' @title plot age group plot
#' @description figure with separate plot for each data frame, plotting incidence or mortality against period
#' @param df dataframe
#' @param fstate federal state
#' @export
plotAgGrid <- function(df, fstate){
  colors <- c("incidence" = "blue", "mortality" = "red", "complete" = "black")
  ggplot(df, aes(x=period)) +
    geom_line(aes(y=incRate, color = "incidence"), size = 1, alpha = 0.7) +
    geom_line(aes(y=mortRate, color= "mortality"), size = 1, alpha= 0.7) +
    facet_wrap(~agegroup) +
    theme(legend.title = element_blank()) +
    theme(text = element_text(size = 15))+
    ylab('Rate per 100,000 persons') +
    geom_vline(xintercept=getCompleteYears(fstate), linetype=2)
}

#' @title plot age group plot
#' @description figure with separate plot for each data frame, plotting mortality divided by incidence against period
#' @param df dataframe
#' @param fstate federal state
#' @export
plotAgMortPerInc <- function(df, fstate){
  ggplot(df, aes(x=period)) +
    geom_line(aes(y=mortRate/incRate, color = "mortality/incidence"), size = 1, alpha = 0.7) +
    facet_wrap(~agegroup) +
    theme(legend.title = element_blank()) +
    theme(text = element_text(size = 15))+
    ylab('Rate per 100,000 persons') +
    if(fstate != "ALL") geom_vline(xintercept=getCompleteYears(fstate), linetype=2)
}
###---------------------------------MODULE----------------------------
AgeGroupPlotUI <- function(id) {
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
    fluidRow(downloadButton(NS(id, 'downloadPlot2'),'Download Plot')),
    plotOutput(NS(id,"plot2")))
}

AgeGroupPlotServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$hei<- renderText(paste('<B>data:</B> ',choice()))
    agDf <- reactive(prepareData(data(),input$state))

    plotVar <- reactive(plotAgGrid(agDf(),input$state))
    output$plot <- renderPlot(plotVar())
    output$downloadPlot <- downloadHandler(
      filename = function(){'agegroup1.pdf'},
      content = function(file){ggsave(file, plot=plotVar(), width=12, height=6, units = "in")}
    )

    plotVar2 <- reactive(plotAgMortPerInc(agDf(),input$state))
    output$plot2 <- renderPlot(plotVar2())
    output$downloadPlot2 <- downloadHandler(
      filename = function(){'agegroup2.pdf'},
      content = function(file){ggsave(file, plot=plotVar2(), width=12, height=6, units = "in")}
    )
    output$all <- renderText("*Only the eight states with complete data in the period 2001 to 2014 are included")
  })
}

