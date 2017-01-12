
library(shiny)
library(ggplot2)
library(dplyr)


function(input, output, session) {

    filter.data <- reactive({

        ##------------------
        ## Filter
        ##------------------

        ## Alder
        alderVec <- min(input$alder):max(input$alder)

        ## Kjonn
        kjonnVec <- if (input$kjonn != 3) {input$kjonn} else { kjonnVec = 1:2 }

        ## Dato (datoFra, datoTil)
        datoFra <- format(input$dato[1])
        datoTil <- format(input$dato[2])
        datoVec <- seq(as.Date(datoFra, format = "%Y-%m-%d"),
                       as.Date(datoTil, format = "%Y-%m-%d"), "day")

        ## Diabetes type 1
        dbType <- input$dbtype
        diaVec <- if (dbType == 1) {dbType} else {diaVec = 1:2}

        ## Registrering (dataValg) Førstegangreg, årskontroll, poliklinisk etc
        dataValg <- input$DataValg
        regVec <- if (dataValg != 4) {dataValg} else {regVec <- 1:3}

        dplyr::filter(data,
                      Alder %in% alderVec,
                      kjonn %in% kjonnVec,
                      innYear %in% datoVec,
                      regValg %in% regVec,
                      diaType1 %in% diaVec)


    })

    output$test <- renderDataTable({
        filter.data()
    })


    output$plot <- renderPlot({

        data <- filter.data()
        dtall <- data %>% group_by(AlderKat) %>%
            tally

        ggplot(dtall, aes(AlderKat, n)) + geom_bar(stat = "identity")

    })

    session$onSessionEnded(stopApp)
}
