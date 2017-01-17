
library(shiny)
library(ggplot2)
library(dplyr)


function(input, output, session) {

    fil.data <- eventReactive(input$go, {

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

        RegData <- dplyr::filter(data,
                                 Alder %in% alderVec,
                                 kjonn %in% kjonnVec,
                                 innYear %in% datoVec,
                                 regValg %in% regVec,
                                 diaType1 %in% diaVec)


        ##-----------------------------
        ## Sammenligne sykehus funksjon
        ##-----------------------------

        sykSamlikFn <- function(x) {
            sykSamlik = c("SykehusKode",
                          "Sykehus",
                          "hba",
                          "diaVarighet",
                          "Variabel")
            RegData$Variabel <- RegData[,x]
            RegDataValg <- RegData %>%
                select_(.dots = sykSamlik)
            return(RegDataValg)
        }



        ## === Kategoriske Variabler (xScale==2) === ##

        ## Alderskategori
        if (input$valgtVar == 2) {
            xScale = 2
            valgtVar = "AlderKat"
            RegDataValg <- sykSamlikFn(valgtVar)
            figT <- "Figur: Fordeling av alder"
            xLab = "Alderskategorier (år)"
            xBreaks = levels(RegDataValg$Variabel)
        }

        ## Kjønn
        if (input$valgtVar == 3) {
            xScale = 2
            valgtVar = "kjonn1"
            RegDataValg <- sykSamlikFn(valgtVar)
            ## levels = 1:2
            ## labels = c("Gutt", "Jente")
            ## RegDataValg$Variabel <- factor(RegDataValg$Variabel, levels = levels, labels = labels)
            figT <- "Figur: Fordeling av kjønn"
            xLab = "Kjønn"
            xBreaks = levels(RegDataValg$Variabel)
        }


        RegDataValg

    })

    ## Lokal og landet
    data.ll <- reactive({

        source("./codes/prosent.R", local = TRUE)

        if (input$RapValg == 2) {

            data <- fil.data() %>%
                mutate(group = ifelse(input$sykehus == SykehusKode, 1, 2)) %>%
                filter(group == 1)
        }

        if (input$RapValg == 1) {

            data <- fil.data() %>%
                mutate(group = 1)
        }

        tab.data <- prosent(data, "Variabel")
        tab.data
    })

    ## Andre sykehus
    data.andre <- reactive({

        source("./codes/prosent.R", local = TRUE)

        if (input$RapValg == 3) {

            data <- fil.data() %>%
                mutate(group = ifelse(input$sykehus == SykehusKode, 1, 2)) %>%
                filter(group == 2)

            tab.data <- prosent(data, "Variabel")
        }
        tab.data
    })

    output$test <- renderDataTable({

        if (input$RapValg %in% 1:2){
            data <- data.ll()
        } else {
           data <- data.andre()
        }
        data
    })

    output$plot <- renderPlot({

        ## if (input$yaksen == 1) {
        ##     yLab <- "Prosent (%)"

        ## }

        ggplot(data.ll(), aes(Variabel, n)) + geom_bar(stat = "identity")

    })

    session$onSessionEnded(stopApp)
}
