#' @import shiny
NULL

#' Runs the shiny application
#' @export
gcaApp <- function(){
    ui <- navbarPage("german cancer",
                     tabPanel("choose data", dataChoiceUI("tab0")),
                     tabPanel("explore data", dataFeedbackUI("tab1")),
                     tabPanel("geographic analysis", geoAnalysisUI("tab2")),
                     tabPanel("cohort analysis", cohortPlotUI("tab3")),
                     tabPanel("agegroup analysis", AgeGroupPlotUI("tab4")),
                     tabPanel("agegroup histograms", histogramPlotUI("tab5")),
                     tabPanel("age standardized analysis",asisPlotUI("tab6")))

        server <- function(input, output, session) {
        data <- dataChoiceServer("tab0")
        dataFeedbackServer("tab1", data)
        geoAnalysisServer("tab2", data)
        cohortPlotServer("tab3", data)
        AgeGroupPlotServer("tab4", data)
        histogramPlotServer("tab5",data)
        asisPlotServer("tab6",data)

    }

shinyApp(ui = ui, server = server)
}

