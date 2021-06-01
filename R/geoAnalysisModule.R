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
    theme(strip.text.x = element_text(size = 12))+
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
    theme(strip.text.x = element_text(size = 12)) +
    ylab(sprintf("%s per 100 k","mortality")) +
    xlab(sprintf("%s", "period")) +
    labs(color="complete")
}

##-------------------------------------module---------------------------------##
geoAnalysisUI <- function(id) {
  fluidPage(
    fluidRow(
      column(width=8,
             selectInput(NS(id,"vary"),
                "Select a y-variable",
                choices=c("incidence","mortality"))),
      column(width=4, align='right',
           htmlOutput(NS(id,"hei")))),
    plotOutput(NS(id,"inc")),
    plotOutput(NS(id,"mort"))
  )
}

geoAnalysisServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$hei<- renderText(paste('<B>data:</B> ',choice()))
    df <- reactive(
      sumOverGroups(data()))
    output$inc <- renderPlot(plotGeoInc(df()))
    output$mort <- renderPlot(plotGeoMort(df()))
  })
}
