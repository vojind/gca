#Geography analysis
#' @import shiny ggplot2
NULL

#' @title summing over groups, specific for geoAnalysis plot
#' @param df dataframe
sumOverGroups <- function(df){
  result <- dplyr::summarise(dplyr::group_by(df,
                                             period = period,
                                             FedState = FedState,
                                             available,
                                             complete),
                             incidence = sum(incidence),
                             mortality = sum(mortality),
                             population =sum(population))
  result <- subset(result, FedState != 'Münster')
  result$incPer100k <- as.integer(with(result, incidence * (1e5/population)))
  result$mortPer100k <- as.integer(with(result, mortality * (1e5/population)))
  return(as.data.frame(result))
}
#' @title display incidence plots arranged as Germany
#' @description display incidence for each state, arranged as germany.
#' @param df dataframe
#' @export
plotGeoInc <- function(df){
  library(geofacet)
  ggplot(df, aes(period, incPer100k, group = 1))+
    geom_point(aes(color=df$complete)) +
    geofacet::facet_geo(~ FedState, grid = 'de_states_grid1', label='name')+
    ggtitle("Incidence") +
    theme(text = element_text(size = 15))+
    ylab(sprintf("%s per 100 k","Incidence"))+
    xlab(sprintf("%s", "period"))+
    labs(color="complete")
}



#' @title display mortality plots arranged as Germany
#' @description display incidence for each state, arranged as germany.
#' @param df dataframe
#' @export
plotGeoMort <- function(df){
  library(geofacet)
  ggplot(df, aes(period, mortPer100k, group = 1))+
    geom_point(aes(color=df$complete)) +
    geofacet::facet_geo(~ FedState, grid = 'de_states_grid1', label='name') +
    ggtitle("Mortality") +
    theme(text = element_text(size = 15))+
    ylab(sprintf("%s per 100 k","mortality")) +
    xlab(sprintf("%s", "period")) +
    labs(color="complete")
}
##-------------------------------------module---------------------------------##
geoAnalysisUI <- function(id) {
  fluidPage(
    fluidRow(
      column(width=8, align='left',
             downloadButton(NS(id, 'downloadInc'),'Download Incidence Plot')),
      column(width=4, align='right',
           htmlOutput(NS(id,"hei")))),
    plotOutput(NS(id,"inc")),
    fluidRow(
      column(width=8, align='left',
             downloadButton(NS(id, 'downloadMort'),'Download Mortality Plot'))),
    plotOutput(NS(id,"mort")),
  )
}

geoAnalysisServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$hei<- renderText(paste('<B>data:</B> ',choice()))

    df <- reactive(
      sumOverGroups(data()))


    incplot  <- reactive(plotGeoInc(df()))
    mortplot <- reactive(plotGeoMort(df()))

    output$inc <- renderPlot(incplot())
    output$mort <- renderPlot(mortplot())

    output$downloadInc <- downloadHandler(
      filename = function(){'test.pdf'},
      content = function(file){ggsave(file, plot=incplot(), width=12, height=7, units="in")}
    )
    output$downloadMort <- downloadHandler(
      filename = function(){'test.pdf'},
      content = function(file){ggsave(file, plot=mortplot(), width=12, height=7, units="in")}
    )
    })
}



