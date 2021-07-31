#' The UI module for choosing dataframes.
#' @param id passed to the module in 'app.R', must match with id passed to server.
dataChoiceUI <- function(id) {
  fluidPage(
    fluidRow(column(width=12, align="center",
      titlePanel("Choose data"))),
    fluidRow(column(width=12, align="center",
                    radioButtons(NS(id,"which"), "Use uploaded or existing data set?",
                 choices = c("uploaded", "existing"),
                 selected= "existing"))),
    fluidRow(column(width=12, align="center",
                    radioButtons(NS(id,"dataOld"), "Choose from existing data",
                 choices=c("prostate", "breast", "trachea")))),
    fluidRow(width=12, align="center",htmlOutput(NS(id,"hei"))),

    column(width=12, align="center",
           div(style= "font-size: 10px; padding: 0px 0px; margin-top:1em",
                 fileInput(NS(id,"upload1"), NULL,  width='15%',placeholder = "incidence")),
           div(style = "font-size: 10px; padding: 0px 0px; margin-top:-3em",
               fileInput(NS(id,"upload2"), NULL, width='15%', placeholder = "population")),
           div(style= "font-size: 10px; padding: 0px 0px; margin-top:-3em",
               fileInput(NS(id,"upload3"), NULL,  width='15%', placeholder = "mortality")),
           div(style= "font-size: 10px; padding: 0px 0px; margin-top:-3em",
               actionButton(NS(id,"submit"), label="Submit")))
  )
}

#' The server module for choosing dataframes.
#' @param id passed to the module in 'app.R', must match with id passed to UI.
#' @return a dataframe for further use
dataChoiceServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    choice <<- reactive(if(input$which=="existing") input$dataOld else "uploaded file")
    datta <- reactive({removeNans(cancerData[[input$dataOld]][[1]])})
    output$hei <- renderText("<B>Upload your own dataset<B>")
    dattaUploaded <- reactive(removeNans(cleanAndCombine(input$upload1$datapath,
                                                         input$upload2$datapath,
                                                         input$upload3$datapath)[[1]]))
    return(reactive(if(input$which=="existing") datta() else dattaUploaded()))
})}
