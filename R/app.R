#' @import shiny
NULL

#' Run shiny application
#'
#' @export
#'
#' @examples gcaApp()
gcaApp <- function(){
    ui <- navbarPage("german cancer",
                     tabPanel("choose data", dataChoiceUI("tab0")),
                     tabPanel("cancer rate plot",asisPlotUI("tab6")),
                     tabPanel("clean data frame", dataFeedbackUI("tab1")),
                     tabPanel("spatial plot", geoAnalysisUI("tab2")),
                     tabPanel("cohort plot", cohortPlotUI("tab3")),
                     tabPanel("age group analysis", AgeGroupPlotUI("tab4")),
                     tabPanel("spatial age group plot", histogramPlotUI("tab5")))

    server <- function(input, output, session) {
    datta <- dataChoiceServer("tab0")
    dataFeedbackServer("tab1", datta)
    geoAnalysisServer("tab2", datta)
    cohortPlotServer("tab3", datta)
    AgeGroupPlotServer("tab4", datta)
    histogramPlotServer("tab5",datta)
    asisPlotServer("tab6",datta)
    }

shinyApp(ui = ui, server = server)
}

