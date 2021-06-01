#' The UI module for choosing dataframes.
#' @param id passed to the module in 'app.R', must match with id passed to server.
dataChoiceUI <- function(id) {
  fluidPage(
    radioButtons(NS(id,"dataOld"), "choose from existing data",
                 choices=c("prostate", "breast")),
    radioButtons(NS(id,"which"), "Use uploaded or existing data set?",
               choices = c("uploaded", "existing"),
                 selected= "existing"),
    fileInput("upload1", "incidence",  width='30%'),
    fileInput("upload2", "population", width='30%'),
    fileInput("upload3", "mortality",  width='30%'),
    actionButton("submit", label="Submit"))
}

#' The server module for choosing dataframes.
#' @param id passed to the module in 'app.R', must match with id passed to UI.
#' @return a dataframe for further use
dataChoiceServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    choice <<- reactive(input$dataOld)
    dataOld <- reactive(removeNans(cancerData[[input$dataOld]][[1]]))

    #dataUpload <-
    #  eventReactive(input$submit,{
    #    total <- removeNans(cleanAndCombine(input$upload1$datapath,
     #                                       input$upload2$datapath,
    #                                        input$upload3$datapath)[[1]])
     #   })
    #return(reactive({if(input$which=='existing')dataOld() else dataUpload()}))
  })
}
