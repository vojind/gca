#' @title prepare data for histogram plot
#' @description prepares data frame for histogram plotting.
#' Selects only complete data and then sums over agegroups
#' @param df dataframe
#' @return dataframe ready for plotting

tweakHistogramData <- function(df){
  df <- subset(df, complete==T)
  ##----------------------------------------------##
  df <- as.data.frame(dplyr::summarise(dplyr::group_by(df,
                                                       agegroup = agegroup,
                                                       FedState = FedState),
                                       incidence = sum(incidence),
                                       mortality = sum(mortality),
                                       population= sum(population)))
  df$incRate <- with(df, incidence*(1e5/population)*as.numeric(weights[as.character(agegroup)]))
  df$mortRate <- with(df, mortality*(1e5/population)*as.numeric(weights[as.character(agegroup)]))
  #_______________________________________________##

  df <- tidyr::gather(df,key = event, value = total, incRate:mortRate)
  df$event <- plyr::revalue(df$event, c("incRate"="incidence", "mortRate"="mortality"))
  return(df)}

#' @title plots incidence and mortality
#' @description plots histogram for each federal state. x-variable is agegroup and y-variable is
#' incidence- and mortality rate
#' @param df dataframe
histogramPlotter <- function(df){
  library(geofacet)
  ggplot(df, aes(agegroup, total, fill=event))+
    geom_bar(stat = "identity", position = 'dodge')+
    geofacet::facet_geo(~ FedState, grid = geofacet::de_states_grid1, label='name') +
    scale_x_discrete(guide = guide_axis(check.overlap=T)) +
    scale_color_manual(labels = c("incidence", "mortality"), values = c("blue", "red"))+
    theme(text = element_text(size = 15))+
    ylab(sprintf("%s per 100 k","rate"))+
    xlab(sprintf("%s", "age group")) +
    theme(legend.title = element_blank())
}

##----------------------module-----------------------
histogramPlotUI <- function(id) {
  fluidPage(
    fluidRow(
      column(width=6, align='left',
            downloadButton(NS(id, 'downloadPlot'),'Download Plot')),
      column(width=6, align='right',
           htmlOutput(NS(id,"hei")))),
    fluidRow(
      column(width=2),
      column(width=8, align='center',plotOutput(NS(id,"plot"), height = "700px")),
      column(width=2)))
    #plotOutput(NS(id,"plot")))
}

histogramPlotServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$hei<- renderText(paste('<B>data:</B> ',choice()))
    df <- reactive(
      tweakHistogramData(data()))
    plotVar <- reactive(histogramPlotter(df()))
    output$plot <- renderPlot(plotVar())
    output$downloadPlot <- downloadHandler(
      filename = function(){'filename.pdf'},
      content = function(file){ggsave(file, plot=plotVar(), width=12, height=6, units = "in")}
    )
    })
}


