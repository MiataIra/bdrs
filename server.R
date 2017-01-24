
library(shiny)
library(ggplot2)
library(highcharter)
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
                        paste0(' Alderskategori: ', minAlder, ' til ', maxAlder, ' år ')
                    },

                    if (kjonn %in% 1:2) {
                        paste0(' Kjønn: ', c('Gutter', 'Jenter')[kjonn])
                    },

                    paste0(' Diabetes: ', c('Type 1', 'Alle typer')[dbType]),

                    paste0(' Periode: ', if (min(RegData$innYear, na.rm = TRUE) > datoFra) {
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
            figT <- "Fordeling av alder"
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
            figT <- "Fordeling av kjønn"
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


    data.123 <- reactive({

        source("./codes/prosent.R", local = TRUE)
        data.inn <- fil.data()$data
        RapValg <- as.integer(input$RapValg)


        ## Lokal
        if (RapValg == 2) {

            ## data <- dplyr::filter(data.inn, SykehusKode == sykehus)

            data <- data.inn %>%
                mutate(group = ifelse(input$sykehus == SykehusKode, 1, 2)) %>%
                filter(group == 1)

            sykNavn <- data$Sykehus[1]
            N <- dim(data)[1]
            sykehusNavn <- paste0(sykNavn, " (N = ", N, ") ")
        }

        ## Landet
        if (RapValg == 1) {

            data <- data.inn %>%
                mutate(group = 1)

            N <- dim(data.inn)[1]
            sykehusNavn <- paste0("Hele landet (N = ", N, ") ")
        }

        ## Andre vs. lokal
        if (RapValg == 3) {

            data.2 <- data.inn %>%
                mutate(group = ifelse(input$sykehus == SykehusKode, 1, 2)) %>%
                filter(group == 2)

            ## data.andre <- dplyr::filter(data.inn, SykehusKode != sykehus)

            andreN <- dim(data.2)[1]
            sykehusAndre <- paste0("Øvrige sykehus (N = ", andreN, ") ")

        }

        ## samler alle data
        if (RapValg %in% 1:2) {
            tab.data.ll <- prosent(data, "Variabel")
        } else {
            tab.data.ll <- NULL
            sykehusNavn <- NULL
            N <- NULL
        }

        if (RapValg == 3) {
            tab.data2 <- prosent(data.2, "Variabel")
        } else {
            tab.data2 <- NULL
            sykehusAndre <- NULL
            andreN <- NULL
        }

        data.list <- list(data.ll = tab.data.ll, data.2 = tab.data2,
                          sykehus1 = sykehusNavn, sykehus2 = sykehusAndre,
                          n1 = N, n2 = andreN)

    })


    output$plot <- renderHighchart({


        rapvalg <- as.integer(input$RapValg)
        xScale <- fil.data()$xScale
        figdata <- data.123()$data.ll
        figdata2 <- data.123()$data.2
        yAksen <- switch(input$yaksen,
                         "Prosent" = 1,
                         "Antall" = 2)

        sykehusNavn <- data.123()$sykehus1
        N <- data.123()$n1
        andreN <- data.123()$n2
        sykehusAndre <- data.123()$sykehus2

        figTitle <- fil.data()$figT
        figSubT <- paste(fil.data()$figTxt, sep = "  ")

        xLab <- fil.data()$xLab
        yLab <- switch(input$yaksen,
                       "Prosent" = "Prosent (%)",
                       "Antall" = "Antall pasienter")

        ## if (input$yaksen == "1") yLab="Prosent (%)"
        ## if (input$yaksen == "2") yLab="Antall pasienter"


        col1 <- "#6699CC"
        col2 <- "#000099"
        coll <- "#999999" #line color


        ##---------------
        ## Figur
        ##---------------


        ## -- Kategoriske variabler --##

        if (rapvalg %in% 1:2 && yAksen == 1 && xScale == 2) {

            figdata$yAksen=as.numeric(sprintf("%.1f", figdata$yAksen))

            fig.plot <- hchart(figdata, "column", hcaes(x = Variabel, y = yAksen),
                               color = col1) %>%
                hc_title(text = figTitle, style = list(color = "black", fontSize = "25px")) %>%
                hc_subtitle(text = figSubT) %>%
                hc_xAxis(title = list(text = xLab)) %>%
                hc_yAxis(title = list(text = yLab))
        }

        if (rapvalg %in% 1:2 && yAksen == 2 && xScale == 2) {

            figdata$yAksen=as.numeric(sprintf("%.f", figdata$yAksen))

            fig.plot <- hchart(figdata, "column", hcaes(x = Variabel, y = yAksen),
                               color = col1) %>%
                hc_title(text = figTitle, style = list(color = "black", fontSize = "25px")) %>%
                hc_subtitle(text = figSubT) %>%
                hc_xAxis(title = list(text = xLab)) %>%
                hc_yAxis(title = list(text = yLab))
        }


        ##-- lokal vs. andre

        if (rapvalg == 3 && yAksen == 1 && xScale == 2) {

            figdata2$yAksen=as.numeric(sprintf("%.1f", figdata2$yAksen))

            figdata1 <- data.123()$data.ll
            figdata1$yAksen=as.numeric(sprintf("%.1f", figdata1$yAksen))


            fig.plot <- highchart() %>%
                hc_title(text = figTitle, style = list(color = "black")) %>%
                hc_subtitle(text = figSubT) %>%
                hc_xAxis(categories = figdata$yAksen, title = list(text = xLab)) %>%
                hc_yAxis(title = list(text = yLab)) %>%
                hc_add_series(figdata1$Variabel)

        }

        if (rapvalg == 3 && yAksen == 2 && xScale == 2) {

            figdata2$yAksen=as.numeric(sprintf("%.f", figdata2$yAksen))

            figdata1 <- data.123()$data.ll
            figdata1$yAksen=as.numeric(sprintf("%.f", figdata1$yAksen))


            fig.plot <- highchart() %>%
                hc_title(text = figTitle, style = list(color = "black")) %>%
                hc_subtitle(text = figSubT) %>%
                hc_xAxis(categories = figdata$yAksen, title = list(text = xLab)) %>%
                hc_yAxis(title = list(text = yLab)) %>%
                hc_add_series(figdata1$Variabel)


        }

        fig.plot


        ##----------------------
        ## ggplot2 figur
        ##---------------------

        ## ## Figur
        ## ffig <- ggplot2::ggplot(NULL, aes(x = Variabel, y = yAksen)) +
        ##     ggplot2::geom_bar(data = figdata, aes(fill = sykehusNavn), stat = "identity") +
        ##     ggplot2::scale_fill_manual(name = " ", values = col1) +
        ##     ggplot2::coord_cartesian(ylim = c(1,ymax)) +
        ##     ggplot2::scale_y_continuous(expand = c(0,0)) +
        ##     ggplot2::labs( x =xLab, y = yLab) +
        ##     ggplot2::ggtitle(bquote(atop(.(""),atop(.(figsubT), ""))))

        ##     ## Theme
        ##     theme1 <- ggplot2::theme(plot.margin = unit(c(txtSpace, 1,1,1), "lines"),
        ##                              plot.title = element_text(hjust = 0, size=15),
        ##                              legend.position = 'top',
        ##                              legend.text = element_text(size = 11),
        ##                              legend.title = element_blank(),
        ##                              legend.box = "horizontal",
        ##                              panel.background = element_blank(),
        ##                              panel.border = element_blank(),
        ##                              panel.grid.major.y = element_line(color = coll, size = .3, linetype = "dashed"),
        ##                              axis.title = element_text(face = "bold", size = 11),
        ##                              axis.ticks.y = element_line(size = .3, color = coll),
        ##                              axis.ticks.x = element_blank(),
        ##                              axis.text = element_text(size = 10),
        ##                              axis.text.y = element_text(vjust = 0),
        ##                              axis.line.x = element_line(size =.5))



        ##     ## ============================
        ##     ## Figure for prosent og antall
        ##     ## ============================


        ##     ## -- Kategoriske variabler --##

        ##     if (rapvalg %in% 1:2 && yAksen == 1 && xScale == 2) {

        ##         figdata$yAksen=as.numeric(sprintf("%.1f", figdata$yAksen))

        ##         regfig <- ffig +
        ##             ggplot2::geom_text(data = figdata, aes(label = yAksen, vjust = -0.25)) +
        ##             theme1
        ##     }

        ##     if (rapvalg %in% 1:2 && yAksen == 2 && xScale == 2) {

        ##         figdata$yAksen=as.numeric(sprintf("%.f", figdata$yAksen))

        ##         regfig <- ffig +
        ##             ggplot2::geom_text(data = figdata, aes(label = yAksen, vjust = -0.25)) +
        ##             theme1

        ##     }

        ##     if (rapvalg == 3 && yAksen == 1 && xScale == 2) {

        ##         figdata2$yAksen=as.numeric(sprintf("%.1f", figdata2$yAksen))

        ##         regfig <- ffig +
        ##             ggplot2::geom_point(data = figdata2, aes(color = sykehusAndre),
        ##                                 shape = 18, size = 6, stat = "identity" ) +
        ##             ggplot2::scale_color_manual(name = " ", values = col2) +
        ##             ggplot2::guides(color = guide_legend(order = 2),
        ##                             fill = guide_legend(order = 1)) +
        ##             theme1

        ##     }

        ##     if (rapvalg == 3 && yAksen == 2 && xScale == 2) {

        ##         figdata2$yAksen=as.numeric(sprintf("%.f", figdata2$yAksen))


        ##         regfig <- ffig +
        ##             ggplot2::geom_point(data = figdata2, aes(color = sykehusAndre),
        ##                                 shape = 18, size = 6, stat = "identity" ) +
        ##             ggplot2::scale_color_manual(name = " ", values = col2) +
        ##             ggplot2::guides(color = guide_legend(order = 2),
        ##                             fill = guide_legend(order = 1)) +
        ##             theme1

        ##     }


        ##     regfig

        ##     ## ggplot(data.ll()$data, aes(Variabel, n)) + geom_bar(stat = "identity")

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


    output$textsub <- renderText({

        subTitle <- paste0(fil.data()$figTxt, collapse = "\n")
        paste0(subTitle)
    })

    output$test <- renderDataTable({

        if (input$RapValg %in% 1:2){
            data <- data.123()$data
        } else {
            data <- data.123()$data2
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
