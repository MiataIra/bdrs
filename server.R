
library(shiny)
library(ggplot2)
library(dplyr)


function(input, output, session) {

    fil.data <- eventReactive(input$go, {

        ##------------------
        ## Filter
        ##------------------

        ## Alder
        minAlder <- min(input$alder)
        maxAlder <- max(input$alder)
        alderVec <- minAlder:maxAlder

        ## Kjonn
        kjonn <- as.integer(input$kjonn)
        kjonnVec <- if (input$kjonn != 3) {input$kjonn} else { kjonnVec = 1:2 }

        ## Dato (datoFra, datoTil)
        datoFra <- format(input$dato[1])
        datoTil <- format(input$dato[2])
        datoVec <- seq(as.Date(datoFra, format = "%Y-%m-%d"),
                       as.Date(datoTil, format = "%Y-%m-%d"), "day")

        ## Diabetes type 1
        dbType <- as.integer(input$dbtype)
        diaVec <- if (dbType == 1) {dbType} else {diaVec = 1:2}

        ## Registrering (dataValg) Førstegangreg, årskontroll, poliklinisk etc
        dataValg <- as.integer(input$DataValg)
        regVec <- if (dataValg != 4) {dataValg} else {regVec <- 1:3}


        RegData <- dplyr::filter(data,
                                 Alder %in% alderVec,
                                 kjonn %in% kjonnVec,
                                 innYear %in% datoVec,
                                 regValg %in% regVec,
                                 diaType1 %in% diaVec)

        ##----------------------------
        ## Figurtekst
        ##----------------------------

        figTxt <- c(paste0('Data for ', paste0(': ', c('Førstegangsregistrering',
                                                       'Årskontroll',
                                                       'Poliklinisk',
                                                       'Alle type kontroller')[dataValg])),
                    if ((minAlder > 0) || (maxAlder < 100)) {
                        paste0('Alderskategori: ', minAlder, ' til ', maxAlder, ' år ')
                    },

                    if (kjonn %in% 1:2) {
                        paste0('Kjønn: ', c('Gutter', 'Jenter')[kjonn])
                    },

                    paste0('Diabetes: ', c('Type 1', 'Alle typer')[dbType]),

                    paste0('Periode: ', if (min(RegData$innYear, na.rm = TRUE) > datoFra) {
                                            format(min(RegData$innYear, na.rm = TRUE), "%d %b %Y")
                                        } else {
                                            format(as.Date(datoFra), format = "%d %b %Y")
                                        }, ' - ',
                           if (max(RegData$innYear, na.rm = TRUE) < datoTil) {
                               format(max(RegData$innYear, na.rm = TRUE), "%d %b %Y")
                           } else {
                               format(as.Date(datoTil), "%d %b %Y")
                           }))


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


        fil.data <- list(data = RegDataValg, figTxt = figTxt, figT = figT, xLab = xLab, xBreaks = xBreaks)

    })

    ## Lokal og landet
    data.ll <- reactive({

        source("./codes/prosent.R", local = TRUE)
        data.inn <- fil.data()$data

        if (input$RapValg == 2) {


            data <- data.inn %>%
                mutate(group = ifelse(input$sykehus == SykehusKode, 1, 2)) %>%
                filter(group == 1)
        }

        if (input$RapValg == 1) {

            data <- data.inn %>%
                mutate(group = 1)
        }

        tab.data <- prosent(data, "Variabel")
        tab.data
    })

    ## Andre sykehus
    data.andre <- reactive({

        source("./codes/prosent.R", local = TRUE)
        data.inn <- fil.data()$data

        if (input$RapValg == 3) {

            data <- data.inn %>%
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

     output$test2 <- renderText({
        text0 <- fil.data()$xLab
        text <- fil.data()$figT

        paste0(text0, '  ---  ', text)
     })


    output$test3 <- renderText({

        text <- fil.data()$figTxt
        paste0(text)
    })


    session$onSessionEnded(stopApp)
}
