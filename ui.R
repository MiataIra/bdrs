
library(shiny)

shinyUI(fluidPage(

    titlePanel("Monitorering for BDR"),

    sidebarLayout(
        sidebarPanel(

            ## Sykehus
            selectInput("sykehus", "Valg sykehus:",
                        choices = list("Ullevål sykehus" = 1,
                                       "Sykehus 2" = 15,
                                       "Sykehus 3" = 18),
                        selected = 1),
            ## DataValg
            selectInput("DataValg", "Valg data:",
                        choices = list("Første gang" = 1, "Årskontroll" = 2, "Poliklinisk" = 3, "Alle" = 4),
                        selected = 1),

            ## RapportValg
            selectInput("Rapvalg", "Rapport valg:",
                        choices = list("Landet" = 1, "Lokal" = 2, "Landet vs. øvrige" = 3),
                        selected = 1),

            ## Dato
            dateRangeInput("dato", "Valg dato fra og til:",
                           start = Sys.Date() - 360, end = Sys.Date()),

            ## DB Type
            radioButtons("dbtype", "Diabetes type:",
                         choices = list("Type1" = 1, "Alle" = 2),
                         selected = 1),

            ## Kjønn
            selectInput("kjonn", "Kjønn",
                         choices = list("Alle" = 3, "Jente" = 2, "Gutt" = 1),
                         selected = 3),


            ## Alder
            numericInput("minAlder", "Minimum Alder:", value = 0),
            numericInput("maxAlder", "Maximum Alder:", value = 25),

            ## Variabel
            selectInput("valgtVar", "Valg variabel for x-aksen:",
                        choices = list("Alder" = 1,
                                       "Alderskategori" = 2,
                                       "Kjonn" = 3),
                        selected = 2),

            ## Y-Aksen
            selectInput("yaksen", "Valg analsyen:",
                        choices = list("Prosent" = 1,
                                       "Antall" = 2,
                                       "HbAlc" = 3,
                                       "DB varighet" = 4)),

            ## Button
            submitButton("Kjør")
        ),


        mainPanel(
        verbatimTextOutput("dato1"))
    )))
