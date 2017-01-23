
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

        fil.data <- list(data = RegDataValg,
                         figTxt = figTxt,
                         figT = figT,
                         xLab = xLab,
                         xBreaks = xBreaks,
                         xScale = xScale)

    })

    ## Lokal og landet
    data.ll <- reactive({

        source("./codes/prosent.R", local = TRUE)
        data.inn <- fil.data()$data
        RapValg <- as.integer(input$RapValg)

        if (RapValg == 2) {

            ## data <- dplyr::filter(data.inn, SykehusKode == sykehus)

            data <- data.inn %>%
                mutate(group = ifelse(input$sykehus == SykehusKode, 1, 2)) %>%
                filter(group == 1)

            sykNavn <- data$Sykehus[1]
            N <- dim(data)[1]
            sykehusNavn <- paste0(sykNavn, " (N = ", N, ") ")
        }

        if (input$RapValg == 1) {

            data <- data.inn %>%
                mutate(group = 1)

            N <- dim(data.inn)[1]
            sykehusNavn <- paste0("Hele landet (N = ", N, ") ")
        }

        tab.data <- prosent(data, "Variabel")
        data.list <- list(data = tab.data, sykehusNavn = sykehusNavn, N = N)
    })

    ## Andre sykehus
    data.andre <- reactive({

        source("./codes/prosent.R", local = TRUE)
        data.inn <- fil.data()$data

        data <- data.inn %>%
            mutate(group = ifelse(input$sykehus == SykehusKode, 1, 2)) %>%
            filter(group == 2)

        ## data.andre <- dplyr::filter(data.inn, SykehusKode != sykehus)

        andreN <- dim(data)[1]
        sykehusAndre <- paste0("Øvrige sykehus (N = ", andreN, ") ")

        tab.data <- prosent(data, "Variabel")

        data.list <- list(data = tab.data, sykehusAndre = sykehusAndre, andreN = andreN)
    })



    output$plot <- renderPlot({


        rapvalg <- as.integer(input$RapValg)
        xScale <- fil.data()$xScale
        figdata <- data.ll()$data
        figdata2 <- data.andre()$data
        yAksen <- switch(input$yaksen,
                         "Prosent" = 1,
                         "Antall" = 2)

        sykehusNavn <- data.ll()$sykehusNavn
        N <- data.ll()$N
        andreN <- data.andre()$andreN
        sykehusAndre <- data.andre()$sykehusAndre

        titBlank <- ""
        figTitle <- c(fil.data()$figT, fil.data()$figTxt)
        figsubT = paste(figTitle, collapse = "\n")
        txtSpace = length(fil.data()$figTxt)

        xLab <- fil.data()$xLab
        yLab <- switch(input$yaksen,
                       "Prosent" = "Prosent (%)",
                       "Antall" = "Antall pasienter")

        ## if (input$yaksen == "1") yLab="Prosent (%)"
        ## if (input$yaksen == "2") yLab="Antall pasienter"


        col1 <- "#6699CC"
        col2 <- "#000099"
        coll <- "#999999" #line color


        ## -- ymax for y-aksen -- ##
        ## Gi god plass mellom legend og figuren
        maxx <-  max(figdata$yAksen, na.rm = TRUE)
        ym <- maxx/6
        ymax <- maxx + ym

        if (input$RapValg == "3") {
            max1 <- max(figdata2$yAksen, na.rm = TRUE)

            if (max1 > maxx) {
                ym1 <- max1/6
                ymax <- max1 + ym1
            } else {
                ymax <- ymax
            }
        }



        ## Figur
        ffig <- ggplot2::ggplot(NULL, aes(x = Variabel, y = yAksen)) +
            ggplot2::geom_bar(data = figdata, aes(fill = sykehusNavn), stat = "identity") +
            ggplot2::scale_fill_manual(name = " ", values = col1) +
            ggplot2::coord_cartesian(ylim = c(1,ymax)) +
            ggplot2::scale_y_continuous(expand = c(0,0)) +
            ggplot2::labs( x =xLab, y = yLab) +
            ggplot2::ggtitle(bquote(atop(.(""),atop(.(figsubT), ""))))

            ## Theme
            theme1 <- ggplot2::theme(plot.margin = unit(c(txtSpace, 1,1,1), "lines"),
                                     plot.title = element_text(hjust = 0, size=15),
                                     legend.position = 'top',
                                     legend.text = element_text(size = 11),
                                     legend.title = element_blank(),
                                     legend.box = "horizontal",
                                     panel.background = element_blank(),
                                     panel.border = element_blank(),
                                     panel.grid.major.y = element_line(color = coll, size = .3, linetype = "dashed"),
                                     axis.title = element_text(face = "bold", size = 11),
                                     axis.ticks.y = element_line(size = .3, color = coll),
                                     axis.ticks.x = element_blank(),
                                     axis.text = element_text(size = 10),
                                     axis.text.y = element_text(vjust = 0),
                                     axis.line.x = element_line(size =.5))



            ## ============================
            ## Figure for prosent og antall
            ## ============================


            ## -- Kategoriske variabler --##

            if (rapvalg %in% 1:2 && yAksen == 1 && xScale == 2) {

                figdata$yAksen=as.numeric(sprintf("%.1f", figdata$yAksen))

                regfig <- ffig +
                    ggplot2::geom_text(data = figdata, aes(label = yAksen, vjust = -0.25)) +
                    theme1
            }

            if (rapvalg %in% 1:2 && yAksen == 2 && xScale == 2) {

                figdata$yAksen=as.numeric(sprintf("%.f", figdata$yAksen))

                regfig <- ffig +
                    ggplot2::geom_text(data = figdata, aes(label = yAksen, vjust = -0.25)) +
                    theme1

            }

            if (rapvalg == 3 && yAksen == 1 && xScale == 2) {

                figdata2$yAksen=as.numeric(sprintf("%.1f", figdata2$yAksen))

                regfig <- ffig +
                    ggplot2::geom_point(data = figdata2, aes(color = sykehusAndre),
                                        shape = 18, size = 6, stat = "identity" ) +
                    ggplot2::scale_color_manual(name = " ", values = col2) +
                    ggplot2::guides(color = guide_legend(order = 2),
                                    fill = guide_legend(order = 1)) +
                    theme1

            }

            if (rapvalg == 3 && yAksen == 2 && xScale == 2) {

                figdata2$yAksen=as.numeric(sprintf("%.f", figdata2$yAksen))


                regfig <- ffig +
                    ggplot2::geom_point(data = figdata2, aes(color = sykehusAndre),
                                        shape = 18, size = 6, stat = "identity" ) +
                    ggplot2::scale_color_manual(name = " ", values = col2) +
                    ggplot2::guides(color = guide_legend(order = 2),
                                    fill = guide_legend(order = 1)) +
                    theme1

            }


            regfig

            ## ggplot(data.ll()$data, aes(Variabel, n)) + geom_bar(stat = "identity")

    })


    output$plot2 <- renderPlot({

        source("./codes/aim.R", local = TRUE)
        dataim <- readRDS("./data/bdraim.RDS")
        dataim$Andel <- as.integer(dataim$Andel)

        source("./codes/mutate-when.R")

        dataim <- dataim %>%
            mutate_when(file == "Oslo", list(region = 1),
                        file == "Elverum", list(region = 18),
                        file == "Harstad", list(region = 15),
                        file == "Molde", list(region = 3),
                        file == "NDS", list(region = 4))
        select <- as.integer(input$sykehus)
        dataim <- filter(dataim, region == select)
        dataim <- select(dataim, c(1,2))
        dataim$Variabel <- as.character(dataim$Variabel)
        aim(dataim, size = 10)

    })


    output$test <- renderDataTable({

        if (input$RapValg %in% 1:2){
            data <- data.ll()$data
        } else {
            data <- data.andre()$data
        }
        data
    })


    ## output$test2 <- renderText({
    ##     text0 <- fil.data()$xLab
    ##     text <- fil.data()$figT

    ##     paste0(text0, '  ---  ', text)
    ## })


    ## output$test3 <- renderText({

    ##     text <- fil.data()$figTxt
    ##     paste0(text)
    ## })


    session$onSessionEnded(stopApp)
}
