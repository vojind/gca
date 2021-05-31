
dataFeedbackUI <- function(id) {
  states <- c("ALL","BB","BE","BW","BY","HB","HE","HH","MV",
              "NI","NW","RP","SH","SL","SN","ST","TH")
  fluidPage(
    selectInput(NS(id,"state"),
                "Select a state",
                choices=states),
    DT::DTOutput(NS(id, "explore"))
  )
}

dataFeedbackServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$explore <-
      DT::renderDT(server=FALSE,{
        DT::datatable(if(input$state == "ALL"){data()}
                  else{stateData <- reactive(subset(data(),FedState == input$state))
                  stateData()},
                  extensions = 'Buttons', options = list(
                    dom = 'Bfrtip',
                    buttons = list('copy', 'print', list(
                      extend = 'collection',
                      buttons = c('csv', 'excel', 'pdf'),
                      text = 'Download'
            ))))
    })
  })
}
