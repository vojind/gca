#Geography analysis
#' @import shiny ggplot2
NULL

#' @title summing over groups, specific for geoAnalysis plot
#' @param df dataframe
sumOverGroups <- function(df){
  df$incRate <- with(df, incidence*(1e5/population)*as.numeric(weights[as.character(agegroup)]))
  df$mortRate <- with(df, mortality*(1e5/population)*as.numeric(weights[as.character(agegroup)]))
  result <- dplyr::summarise(dplyr::group_by(df,
                                             period,
                                             FedState,
                                             available,
                                             complete),
                             incidence = sum(incidence),
                             mortality = sum(mortality),
                             population =sum(population),
                             incRate =sum(incRate),
                             mortRate=sum(mortRate))
  result <- subset(result, FedState != 'Münster')
  return(as.data.frame(result))
}

#' @title display incidence plots arranged as Germany
#' @description display incidence for each state, arranged as germany.
#' @param df dataframe
#' @export
plotGeo <- function(df){
  library(geofacet)
  ggplot(df, aes(period, group = 1))+
    geom_point(aes(y=incRate, colour="incidence",shape=df$complete), size=1.5) +
    geom_point(aes(y=mortRate, colour="mortality",shape=TRUE), size=1.5) +
    geofacet::facet_geo(~ FedState, grid = 'de_states_grid1', label='name')+
    scale_shape_manual(labels=c(FALSE,TRUE), values = c(1,16))+
    ggtitle("Incidence and Mortality") +
    theme(text = element_text(size = 13))+
    theme(legend.text=element_text(size=13), legend.title=element_text(size=13))+
    ylab(sprintf("%s per 100 k","counts"))+
    xlab(sprintf("%s", "period"))+
    labs(color="", shape="complete")
}

##-------------------------------------module---------------------------------##
geoAnalysisUI <- function(id) {
  fluidPage(
    fluidRow(
      column(width=8, align='left',
             downloadButton(NS(id, 'download'),'Download Plot')),
      column(width=4, align='right',
           htmlOutput(NS(id,"hei")))),
    fluidRow(
      column(width=2),
      column(width=8, align='center',plotOutput(NS(id,"plot"), height = "700px")),
      column(width=2)
      )
  )
}

geoAnalysisServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$hei<- renderText(paste('<B>data:</B> ',choice()))
    df <- reactive(sumOverGroups(data()))
    plotVar  <- reactive(plotGeo(df()))
    output$plot <- renderPlot(plotVar())
    output$download <- downloadHandler(
      filename = function(){'plotGeo.pdf'},
      content = function(file){ggsave(file, plot=plotVar(), width=10, height=10, units="in")}
      )
    })
}



