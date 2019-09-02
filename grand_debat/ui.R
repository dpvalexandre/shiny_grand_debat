library(shiny)
library(tidyverse)
library(DT)
library(tools)
library(shinydashboard)
library(dashboardthemes)


grand_debat <- read_csv(file = "grand_debat.csv")



ui <- dashboardPage(
  
  
  dashboardHeader(disable = TRUE),
  
  
  dashboardSidebar(disable =TRUE),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
    ),
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    navbarPage("Le grand débat, les résultats",
               tabPanel("par Commune",
                        
                        column(width = 3,
                               box(title = "Explication du traitement des données",
                                   width = NULL,
                                   div(
                                     p("Bonjour et bienvenue sur ce dashboard destiné à présenter quelques uns des résultats du grand débat. Ce dashboard constitue mon premier projet sur R shiny et a été conçu dans un but d'apprentissage. Les résultats sont présentés de la manière la plus neutre possible et n'ont pas vocation à soutenir ou revendiquer une quelconque idée politique."),
                                     p("Les données sont issues des", a("données ouvertes du Grand Débat National", href = "https://www.data.gouv.fr/fr/datasets/donnees-ouvertes-du-grand-debat-national/", target = "_blank"), "Elle ont été traitée par mes soins en R puis présentées ici avec R shiny."),
                                     p("Par facilité, j'ai choisi de ne rendre compte que du résultat des questions fermées. Ainsi par exemple, il n'y a pas ici de résultat par questions en ce qui concerne la fiscalité et les dépenses publiques. Lorsque des pourcentages sont présentés, c'est toujours en excluant les absences de réponses ou les \"ne sais pas\""),
                                     p("Cette première page propose des résultats chiffrés par commune. Vous pouvez choisir une ou plusieurs communes dans la box ci-dessous par nom de commune ou code postal. Certains nom de communes sont groupés avec les autres lorsque les communes partagent le même code postal."),
                                     p("PARIS, LYON et MARSEILLE aggregent les résultats de l'ensemble des arrondissements.")
                                   )
                               ),
                               
                               box(title = "Choisissez une ou plusieurs communes puis cliquez sur \"Afficher les résultats\"",
                                   solidHeader = TRUE,
                                   width = NULL,
                                   selectInput(inputId = "select_city_name", label = "Commune par nom (plusieurs choix possibles)" , choices =  c(select(grand_debat, Nom_commune)%>% filter(Nom_commune != "NA")%>%unique(), "France entière"), multiple = TRUE, selectize = TRUE),
                                   selectInput(inputId = "select_city_zip_code", label = "Commune par code postal (plusieurs choix possibles)" , choices = unique(grand_debat$authorZipCode), multiple = TRUE, selectize = TRUE),
                                   checkboxGroupInput("choix_du_sujet", "Choisissez les sujets dont vous souhaitez voir les résultats", choices= c("Démocratie et citoyenneté", "La transition écologique", "L'organisation de l'État et des services publiques","La fiscalité et les dépenses publiques"), selected = c("Démocratie et citoyenneté", "La transition écologique", "L'organisation de l'État et des services publiques","La fiscalité et les dépenses publiques")),
                                   actionButton(label = "Afficher les résultats", inputId = "resultats")
                               )
                               
                        ),
                        column(width = 9,
                               #the condition in the conditional panel is in javascript
                               conditionalPanel( condition = "input.choix_du_sujet.includes(`Démocratie et citoyenneté`)",
                                                 width = NULL,
                                                 
                                                 fluidRow(width = NULL,
                                                          box(h3("Démocratie et citoyenneté"), width = 9),
                                                          valueBoxOutput(outputId = "nb_contributions_democratie", width = 2)
                                                 ),
                                                 fluidRow(width = NULL,
                                                          infoBoxOutput(outputId = "democratie1", width = 3),
                                                          infoBoxOutput(outputId = "democratie2", width = 3),
                                                          infoBoxOutput(outputId = "democratie3", width = 3),
                                                          infoBoxOutput(outputId = "democratie4", width = 3)
                                                 ),
                                                 fluidRow(width = NULL,
                                                          infoBoxOutput(outputId = "democratie5", width = 3),
                                                          infoBoxOutput(outputId = "democratie6", width = 3),
                                                          infoBoxOutput(outputId = "democratie7", width = 3)
                                                 )
                               ),
                               conditionalPanel( condition = "input.choix_du_sujet.includes(`La transition écologique`)",
                                                 width = NULL,
                                                 fluidRow(width = NULL,
                                                          box(h3("La transition écologique"), width = 9),
                                                          valueBoxOutput(outputId = "nb_contributions_transition_ecologique", width = 2)
                                                 ),
                                                 fluidRow(width = NULL,
                                                          infoBoxOutput(outputId = "ecologie1", width = 3),
                                                          infoBoxOutput(outputId = "ecologie2", width = 3),
                                                          infoBoxOutput(outputId = "ecologie3", width = 3),
                                                          infoBoxOutput(outputId = "ecologie4", width = 3)
                                                 )
                               ),
                               conditionalPanel( condition = "input.choix_du_sujet.includes(`L'organisation de l'État et des services publiques`)",
                                                 width = NULL,
                                                 fluidRow(width = NULL,
                                                          box(h3("L'organisation de l'État et des services publiques"), width = 9),
                                                          valueBoxOutput(outputId = "nb_contributions_orga_eta", width = 2)
                                                 ),
                                                 fluidRow(width = NULL,
                                                          infoBoxOutput(outputId = "orga_etat1", width = 3),
                                                          infoBoxOutput(outputId = "orga_etat2", width = 3),
                                                          infoBoxOutput(outputId = "orga_etat3", width = 3),
                                                          infoBoxOutput(outputId = "orga_etat4", width = 3)
                                                 ),
                                                 fluidRow(width = NULL,
                                                          infoBoxOutput(outputId = "orga_etat5", width = 3),
                                                          infoBoxOutput(outputId = "orga_etat6", width = 3),
                                                          infoBoxOutput(outputId = "orga_etat7", width = 3),
                                                          infoBoxOutput(outputId = "orga_etat8", width = 3)
                                                 )
                               ),
                               conditionalPanel( condition = "input.choix_du_sujet.includes(`La fiscalité et les dépenses publiques`)",
                                                 width = NULL,
                                                 fluidRow(width = NULL,
                                                          box(h3("La fiscalité et les dépenses publiques"), width = 9),
                                                          valueBoxOutput(outputId = "nb_contributions_fiscalite", width = 2)
                                                 )
                               )
                               
                        )
               )

    )
  )
)