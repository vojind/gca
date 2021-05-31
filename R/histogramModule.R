#' @title prepare data for histogram plot
#' @description prepares data frame for histogram plotting.
#' Selects only complete data and then sums over agegroups
#' @param df dataframe
#' @return dataframe ready for plotting
tweakHistogramData <- function(df){
  df <- subset(df, complete==T)
  df <- as.data.frame(dplyr::summarise(dplyr::group_by(df,
                                         agegroup = agegroup,
                                         FedState = FedState),
                           incidence = sum(incidence),
                           mortality = sum(mortality),
                           population= sum(population)))
  df$incPer100k  <- df$incidence/df$population*1e5
  df$mortPer100k <- df$mortality/df$population*1e5
  df <- tidyr::gather(df,key = event, value = total, incPer100k:mortPer100k)
  df$event <- plyr::revalue(df$event, c("incPer100k"="incidence", "mortPer100k"="mortality"))
  return(df)}

#' @title plots incidence and mortality
#' @description plots histogram for each federal state. x-variable is agegroup and y-variable is
#' incidence- and mortality rate
#' @param df
histogramPlotter <- function(df)
  ggplot(df, aes(agegroup, total, fill=event))+
    geom_bar(stat = "identity", position = 'dodge')+
    geofacet::facet_geo(~ FedState, grid = "de_states_grid1", label='name') +
    scale_x_discrete(guide = guide_axis(check.overlap=T)) +
    scale_color_manual(labels = c("incidence", "mortality"), values = c("blue", "red"))+
    ylab(sprintf("%s per 100 k","rate"))+
    xlab(sprintf("%s", "age group")) +
    theme(legend.title = element_blank())

##----------------------module-----------------------
histogramPlotUI <- function(id) {
    plotOutput(NS(id,"plot"))
}

histogramPlotServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    df <- reactive(
      tweakHistogramData(data()))
    output$plot <- renderPlot(histogramPlotter(df()))
    })
}


