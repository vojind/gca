
#' @title convert agegroups
#' @description convert agegroups to median integer
#' @param string age group string to be converted to an int

convertInterval <- function(string){
  string <- as.character(string)
  chars <- strsplit(string,' ')[[1]]
  num <- strtoi(chars[1]) + strtoi(chars[length(chars)])
  if(is.na(num)){
    temp <- unname(sapply(chars, strtoi))
    return(temp[!is.na(temp)])
  }
  else return(as.integer(num/2))
}

#' @title prepare data for cohort analysis
#' @description data preparation similar to PrepareData(), but with a few extra steps
#' @param data dataframe
#' @param fstate abbrevation for federal state
tweakCohortData <- function(data, fstate){
  years <- getCompleteYears(fstate)
  df <- prepareData(data, fstate)
  df <- subset(df, years[1] <= period & period <= years[2])
  #df <- subset(df, FedState %in% completeStates)
  df$age           <- as.factor(sapply(df$agegroup, FUN=convertInterval))
  df$birthYear     <- df$period - as.integer(as.character(df$age))
  return(df)}

#' @title cohort plots - incidence
#' @description plot cohort plots for incidence data
#' @param df dataframe
#' @export
plotCohortInc <- function(df){
  one <-
    ggplot(df, aes(birthYear, y=incPer100k, colour=age)) +
    geom_line() +
    theme_bw() +
    ylab('Incidence rate per 100,000 persons') +
    xlab('Year of Birth') +
    xlim(1915,1965)+
    directlabels::geom_dl(aes(label = age), method = list(directlabels::dl.trans(x = x + 0.2), "last.points", cex = 0.8))

  two <-
    ggplot(df, aes(period, y=incPer100k, colour=age)) +
    geom_line() +
    theme_bw() +
    ylab('Incidence rate per 100,000 persons')+
    xlab('Year of Death') +
    directlabels::geom_dl(aes(label = age), method = list(directlabels::dl.trans(x = x + 0.2), "last.points", cex = 0.8))
  gridExtra::grid.arrange(one, two, ncol=2)}

#' @title cohort plots - mortality
#' @description plot cohort plots for mortality
#' @param df dataframe
#' @export
plotCohortMort <- function(df){
  three <-
    ggplot(df, aes(birthYear, y=mortPer100k, colour=age)) +
    geom_line() +
    theme_bw() +
    ylab('Mortality rate per 100,000 persons') +
    xlab('Year of Birth')+
    xlim(1915,1965)+
    directlabels::geom_dl(aes(label = age), method = list(directlabels::dl.trans(x = x + 0.2), "last.points", cex = 0.8))

  four <-
    ggplot(df, aes(period, y=mortPer100k, colour=age)) +
    geom_line() +
    theme_bw() +
    ylab('Mortality per 100,000 persons')+
    xlab('Year of Death')+
    directlabels::geom_dl(aes(label = age), method = list(directlabels::dl.trans(x = x + 0.2), "last.points", cex = 0.8))

  gridExtra::grid.arrange(three, four, ncol=2)
}

###--------------module functions--------------######

cohortPlotUI <- function(id) {
  states <-
    c("ALL","BB","BE","BW","BY","HB","HE","HH","MV",
      "NI","NW","RP","SH","SL","SN","ST","TH")
  fluidPage(
    fluidRow(
    column(width=6,
           selectInput(NS(id,"state"),
                       "Select a state",
                       choices=states)),
    column(width=6, align='right',
           htmlOutput(NS(id,"hei")))),
    fluidRow(downloadButton(NS(id, 'downloadInc'),'Download Incidence Plot')),
    plotOutput(NS(id,"inc")),
    fluidRow(downloadButton(NS(id, 'downloadMort'),'Download Mortality Plot')),
    plotOutput(NS(id,"mort"))
  )
}


cohortPlotServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$hei<- renderText(paste('<B>data:</B> ',choice()))
    df <- reactive(
      tweakCohortData(data(),input$state))

    incplott  <- reactive(plotCohortInc(df()))
    mortplott <- reactive(plotCohortMort(df()))

    output$inc <- renderPlot(incplott())
    output$mort <- renderPlot(mortplott())

    output$downloadInc <- downloadHandler(
      filename = function(){'cohortInc.pdf'},
      content = function(file){ggsave(file, plot=incplott(), width=12, height=6, units="in")}
    )
    output$downloadMort <- downloadHandler(
      filename = function(){'cohortMort.pdf'},
      content = function(file){ggsave(file, plot=mortplott(), width=12, height=6, units="in")}
    )
  })
  }
