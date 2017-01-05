
library(shiny)
library(bdr)

data <- bdr::rfile

shinyServer(function(input, output) {

        output$dato1 <- renderText({
            DatoFra <- input$dato[1]
            paste0("DatoFra er", DatoFra)})

})
