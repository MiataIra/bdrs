
library(shiny)
library(shinythemes)

fluidPage(
    theme = shinytheme("spacelab"),
    tags$h2("Monitorering for BDR"),
    tags$h4("Nasjonalt medisinsk kvalitetsregister for barne- og ungdomsdiabetes"),

    sidebarLayout(
        sidebarPanel(

            ## RapportValg
            selectInput("RapValg", "Rapport valg:",
                        choices = list("Landet" = 1, "Lokal" = 2, "Landet vs. øvrige" = 3),
                        selected = 1),
            conditionalPanel(
                condition = "input.RapValg != 1",
                ## Sykehus
                selectInput("sykehus", "Valg sykehus:",
                            choices = list("Ullevål sykehus" = 1,
                                           "Sykehuset i Vestfold" = 15,
                                           "Haugesund sjukehus" = 18,
                                           "St. Olavs Hospital" = 3,
                                           "Sykehuset Levanger" = 4),
                            selected = 1)),
            ## DataValg
            selectInput("DataValg", "Valg data:",
                        choices = list("Første gang" = 1, "Årskontroll" = 2, "Poliklinisk" = 3, "Alle" = 4),
                        selected = 1),

            ## Dato
            dateRangeInput("dato", "Valg dato fra og til:",
                           start = Sys.Date() - 360, end = Sys.Date(), separator = "til", language = "no"),

            ## DB Type
            radioButtons("dbtype", "Diabetes type:",
                         choices = list("Type1" = 1, "Alle" = 2),
                         selected = 1),

            ## Kjønn
            selectInput("kjonn", "Kjønn",
                        choices = list("Alle" = 3, "Jente" = 2, "Gutt" = 1),
                        selected = 3),


            ## Alder
            sliderInput("alder", "Min og Maks Alder",
                        min = 0, max = 30, step = 1, value = c(0, 20)),
            ## numericInput("minAlder", "Minimum Alder:", value = 0, min = 0),
            ## numericInput("maxAlder", "Maximum Alder:", value = 25, min = 1),

            ## Variabel
            selectInput("valgtVar", "Valg variabel for x-aksen:",
                        choices = list("Alder" = 1,
                                       "Alderskategori" = 2,
                                       "Kjonn" = 3),
                        selected = 2),

            ## Y-Aksen
            selectInput("yaksen", "Valg analsyen:",
                        choices = list("Prosent",
                                       "Antall",
                                       "HbAlc",
                                       "DB varighet")),

            ## Button
            actionButton("go", "Oppdatere", icon("refresh"), #"floppy-o" - icon for lagring
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),


    mainPanel(
        tabsetPanel(type = "tab",
                    tabPanel("Figur",
                             h3(""),
                             plotOutput("plot"),
                             textOutput("test2")),

                    tabPanel("Tabell",
                             h3("Tabell for figuren"),
                             h4(""),
                             dataTableOutput("test"),
                             textOutput("test3"))))

    ))
