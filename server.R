
library(shiny)
library(dplyr)
library(lazyeval)


function(input, output, session) {

    filter.data <- reactive({

        ##------------------
        ## Filter
        ##------------------

        '%i%' <- intersect

        indAld <- which(data$Alder >= min(input$alder) & data$Alder <= max(input$alder))

        indKjonn <- if (input$kjonn != 3) {which(data$kjonn == input$kjonn)
                    } else {
                        indKjonn <- 1:2
                    }

        indDato <- which(data$innYear >= format(input$dato[1]) & data$innYear <= format(input$dato[2]))




        ## Alder
        alderVec <- min(input$alder):max(input$alder)

        ## Kjonn
        kjonnVec <- if (input$kjonn != 3) {input$kjonn} else { kjonnVec = 1:2 }

        ## Dato (datoFra, datoTil)
        ## datoVec <- seq(as.Date(datoFra, format = "%Y-%m-%d", origin = "1899-12-30"),
        ##                as.Date(datoTil, format = "%Y-%m-%d", origin = "2899-12-30"), "day")
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

        fileSelect <- lazyeval::interp(~var1 %in% alderVec &
                                           var2 %in% kjonnVec &
                                           var3 %in% datoVec &
                                           var4 %in% regVec &
                                           var5 %in% diaVec,
                                       .values = list(var1 = as.name("Alder"),
                                                      var2 = as.name("kjonn"),
                                                      var3 = as.name("innYear"),
                                                      var4 = as.name("regValg"),
                                                      var5 = as.name("diaType1")))

        ## Filtret data
        data <- dplyr::filter_(.data = data, .dots = fileSelect)
        data

    })

    output$test <- renderText({
        filter.data()
    })



    session$onSessionEnded(stopApp)
}
