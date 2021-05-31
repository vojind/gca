#Geography analysis
#' @import shiny ggplot2
NULL

#' @title summing over groups, specific for geoAnalysis plot
#' @param df dataframe
#' @param varx x-variable
#' @param vary y-variable
sumOverGroups <- function(df,varx,vary){
  if(varx=="period"){
    result <- dplyr::summarise(dplyr::group_by(df,
                                               varx = !!rlang::sym(varx),
                                               FedState = FedState,
                                               available,
                                               complete),
                               vary = sum(!!rlang::sym(vary)),
                               population =sum(population))}
  else{
    result <- dplyr::summarise(dplyr::group_by(df,
                                               varx = !!rlang::sym(varx),
                                               FedState = FedState),
                               vary = sum(!!rlang::sym(vary)),
                               population =sum(population))}

  result <- subset(result, FedState != 'Münster')
  result$varyPerCapita <- as.integer(with(result, vary * (1e5/population)))
  result <<- result
  return(result)
}

#' @title display plots arranged as Germany
#' @description display incidence or mortality for each state, arranged as germany.
#' @param df dataframe
#' @param varxString x-variable, used for xlabel
#' @param varyString y-variable, used for ylabel
#' @export
plotter <- function(df, varxString, varyString){
  ggplot(df, aes(varx, varyPerCapita, group = 1))+
    geom_point() +
    (if(is.null(df$complete)) geom_point(color=3) else geom_point(color = (1 + df$available + df$complete))) +
    geofacet::facet_geo(~ FedState, grid = "de_states_grid1", label='name') +
    ggtitle("German Cancer Data") +
    ylab(sprintf("%s per 100 k",varyString))+
    xlab(sprintf("%s", varxString)) +
    theme(strip.text.x = element_text(size = 6), plot.title = element_text(hjust = 0.5))
}

geoAnalysisUI <- function(id) {
  sidebarLayout(
    sidebarPanel(
      selectInput(NS(id,"vary"),
                "Select a y-variable",
                choices=c("incidence","mortality","population")),

      selectInput(NS(id,"varx"),
                "Select a x-variable",
                choices=c("period","agegroup")),
      width = 3),

    mainPanel(plotOutput(NS(id,"geo")), width = 9),
    position = c("left", "right"),
    fluid = TRUE
  )
}


geoAnalysisServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    df <- reactive(
      sumOverGroups(data(),input$varx,input$vary))
    output$geo <- renderPlot(plotter(df(), input$varx, input$vary))
  })
}
