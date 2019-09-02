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
                                     p("Par facilité, j'ai choisis de ne rendre compte que du résultat des questions fermées. Ainsi par exemple, il n'y a pas ici de résultat par questions en ce qui concerne la fiscalité et les dépenses publiques. Lorsque des pourcentages sont présentés, c'est toujours en excluant les absences de réponses ou les \"ne sais pas\""),
                                     p("Cette première page propose des résultats chiffrés par commune. Vous pouvez choisir une ou plusieurs communes dans la box ci-dessous par nom de commune ou code postal. Certains nom de communes sont groupés avec les autres lorsque les communes partagent le même code postal."),
                                     p("PARIS, LYON et MARSEILLE aggregent les résultats de l'ensemble des arrondissements.")
                                   )
                               ),
                               
                               box(title = "Choisissez une ou plusieurs communes puis cliquez sur \"Afficher les résultats\"",
                                   solidHeader = TRUE,
                                   width = NULL,
                                   selectInput(inputId = "select_city_name", label = "Commune par nom (plusieurs choix possibles)" , choices =  c(select(grand_debat, Nom_commune)%>% filter(Nom_commune != "NA")%>%unique(), "France entière"), multiple = TRUE, selectize = TRUE, selected = "France entière" ),
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
               ),
  tabPanel("sur la carte",
           column(width = 3,
                  box(title = "Explication du traitement des données",
                      width = NULL,
                      div(
                        p("Bonjour et bienvenue sur ce dashboard destiné à présenter quelques uns des résultats du grand débat. Ce dashboard constitue mon premier projet sur R shiny et a été conçu dans un but d'apprentissage. Les résultats sont présentés de la manière la plus neutre possible et n'ont pas vocation à soutenir ou revendiquer une quelconque idée politique."),
                        p("Les données sont issues des", a("données ouvertes du Grand Débat National", href = "https://www.data.gouv.fr/fr/datasets/donnees-ouvertes-du-grand-debat-national/", target = "_blank"), "Elle ont été traitée par mes soins en R puis présentées ici avec R shiny."),
                        p("Par facilité, j'ai choisis de ne rendre compte que du résultat des questions fermées. Ainsi par exemple, il n'y a pas ici de résultat par questions en ce qui concerne la fiscalité et les dépenses publiques. Lorsque des pourcentages sont présentés, c'est toujours en excluant les absences de réponses ou les \"ne sais pas\""),
                        p("Cette première page propose des résultats chiffrés par commune. Vous pouvez choisir une ou plusieurs communes dans la box ci-dessous par nom de commune ou code postal. Certains nom de communes sont groupés avec les autres lorsque les communes partagent le même code postal."),
                        p("PARIS, LYON et MARSEILLE aggregent les résultats de l'ensemble des arrondissements.")
                      )
                  )
                  
           )
  )
    )
)
)

server <- function(input, output) {
  
  observeEvent(input$resultats,{
    withProgress(message ="Calcul des données" , min =0, max = 1, value = 0, {
      
      city_chosen<- reactive({
        isolate({
          if(input$select_city_name == "France entière"){
            grand_debat_filtered <- grand_debat}else if (input$select_city_name == "PARIS"){
              grand_debat_filtered <-grand_debat[str_which(string = grand_debat$Nom_commune, pattern ="^PARIS(?!OT)"),]
            }else if(input$select_city_name == "LYON"){
              grand_debat_filtered <-grand_debat[str_which(string = grand_debat$Nom_commune, pattern ="^LYON"),]
            }else if(input$select_city_name == "MARSEILLE"){
              grand_debat_filtered <-grand_debat[str_which(string = grand_debat$Nom_commune, pattern ="^MARSEILLE"),]
            }
          
          else{
            grand_debat_filtered <- grand_debat%>% filter(authorZipCode %in% input$select_city_zip_code | Nom_commune %in% input$select_city_name)}
          
          return(grand_debat_filtered)
        }) })
      
      incProgress(amount = 1/24)
      
      output$nb_contributions_democratie <- renderValueBox(
        valueBox(
          p("Nb contributions"), paste0(city_chosen() %>% filter(reference=="Démocratie et citoyenneté")%>% count()),
          color = "red"
        )
      )
      
      incProgress(amount = 1/24)
      
      output$democratie1 <- renderInfoBox(
        infoBox(
          icon = icon("briefcase"),
          p("En dehors des élus politiques, faut-il donner un rôle plus important aux associations et aux organisations syndicales et professionnelles ?"),
          paste0( "Oui à ", city_chosen() %>% filter(reference=="Démocratie et citoyenneté")%>%
                    pull(`En dehors des élus politiques, faut-il donner un rôle plus important aux associations et aux organisations syndicales et professionnelles ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$democratie2 <- renderInfoBox(
        infoBox(
          icon = icon("user-times"),
          p("Le non-cumul des mandats instauré en 2017 pour les parlementaires (députés et sénateurs) est :"),
          paste0( "A ", city_chosen() %>% filter(reference=="Démocratie et citoyenneté")%>%
                    pull("Le non-cumul des mandats instauré en 2017 pour les parlementaires (députés et sénateurs) est :")%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% une bonne chose")
          
        )
      ) 
      
      incProgress(amount = 1/24)
      
      output$democratie3 <- renderInfoBox(
        infoBox(
          icon = icon("male"),
          p("Pensez-vous qu'il serait souhaitable de réduire le nombre d'élus (hors députés et sénateurs)"),
          paste0( "Oui à ", city_chosen() %>% filter(reference=="Démocratie et citoyenneté")%>%
                    pull(`Pensez-vous qu'il serait souhaitable de réduire le nombre d'élus (hors députés et sénateurs) ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
          
        )
      ) 
      
      incProgress(amount = 1/24)
      
      output$democratie4 <- renderInfoBox(
        infoBox(
          icon = icon("personbooth"),
          p("Faut-il prendre en compte le vote blanc ?"),
          paste0( "Oui à ", city_chosen() %>% filter(reference=="Démocratie et citoyenneté")%>%
                    pull(`Faut-il prendre en compte le vote blanc ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      incProgress(amount = 1/24)
      
      output$democratie5 <- renderInfoBox(
        infoBox(
          icon = icon("bullhorn"),
          p("Faut-il faciliter le déclenchement du référendum d'initiative partagée (le RIP est organisé à l'initiative de membres du Parlement soutenu par une partie du corps électoral) qui est applicable depuis 2015 ?"),
          paste0( "Oui à ", city_chosen() %>% filter(reference=="Démocratie et citoyenneté")%>%
                    pull(`Faut-il faciliter le déclenchement du référendum d'initiative partagée (le RIP est organisé à l'initiative de membres du Parlement soutenu par une partie du corps électoral) qui est applicable depuis 2015 ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$democratie6 <- renderInfoBox(
        infoBox(
          icon = icon("landmark"),
          p("Faut-il  transformer nos assemblées, dont le Sénat, conseil économique, social et environnemental?"),
          paste0( "Oui à ", city_chosen() %>% filter(reference=="Démocratie et citoyenneté")%>%
                    pull(`Faut-il les transformer ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      
      incProgress(amount = 1/21)
      
      output$democratie7 <- renderInfoBox(
        infoBox(
          icon = icon("hand-holding-usd"),
          p("Pensez-vous qu'il faille instaurer des contreparties aux différentes allocations de solidarité ?"),
          paste0( "Oui à ", city_chosen() %>% filter(reference=="Démocratie et citoyenneté")%>%
                    pull(`Pensez-vous qu'il faille instaurer des contreparties aux différentes allocations de solidarité ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$nb_contributions_transition_ecologique <- renderValueBox(
        valueBox(
          p("Nb contributions"), paste0(paste0(city_chosen() %>% filter(reference=="La transition écologique") %>% count())),
          color = "olive"
        )
      )
      
      incProgress(amount = 1/24)
      
      output$ecologie1 <- renderInfoBox(
        infoBox(
          icon = icon("thermometer"),
          p("Diriez-vous que votre vie quotidienne est aujourd'hui touchée par le changement climatique ?"),
          paste0( "Oui à ", city_chosen()%>%
                    filter(reference=="La transition écologique")%>%
                    pull(`Diriez-vous que votre vie quotidienne est aujourd'hui touchée par le changement climatique ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$ecologie2 <- renderInfoBox(
        infoBox(
          icon = icon("leaf"),
          p("À titre personnel, pensez-vous pouvoir contribuer à protéger l'environnement ?"),
          paste0( "Oui à ", city_chosen()%>%
                    filter(reference=="La transition écologique")%>%
                    pull(`À titre personnel, pensez-vous pouvoir contribuer à protéger l'environnement ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$ecologie3 <- renderInfoBox(
        infoBox(
          icon = icon("fire"),
          p("Par rapport à votre mode de chauffage actuel, pensez-vous qu'il existe des solutions alternatives plus écologiques ?"),
          paste0( "Oui à ", city_chosen()%>%
                    filter(reference=="La transition écologique")%>%
                    pull(`Par rapport à votre mode de chauffage actuel, pensez-vous qu'il existe des solutions alternatives plus écologiques ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$ecologie4 <- renderInfoBox(
        infoBox(
          icon = icon("bicycle"),
          p("Avez-vous pour vos déplacements quotidiens la possibilité de recourir à des solutions de mobilité alternatives à la voiture individuelle comme les transports en commun, le covoiturage, l'auto-partage, le transport à la demande, le vélo, etc. ?"),
          paste0( "Oui à ", city_chosen()%>%
                    filter(reference=="La transition écologique")%>%
                    pull(`Avez-vous pour vos déplacements quotidiens la possibilité de recourir à des solutions de mobilité alternatives à la voiture individuelle comme les transports en commun, le covoiturage, l'auto-partage, le transport à la demande, le vélo, etc. ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$nb_contributions_orga_eta <- renderValueBox(
        valueBox(
          p("Nb contributions"), paste0(city_chosen()%>% filter(reference=="L'organisation de l'état")%>% count()),
          color = "yellow"
        )
      )
      
      incProgress(amount = 1/24)
      
      output$orga_etat1 <- renderInfoBox(
        infoBox(
          icon = icon("city"),
          p("Selon vous, l'Etat doit-il aujourd'hui transférer de nouvelles missions aux collectivités territoriales ?"),
          paste0( "Oui à ", city_chosen()%>%
                    filter(reference=="L'organisation de l'état")%>%
                    pull(`Selon vous, l'Etat doit-il aujourd'hui transférer de nouvelles missions aux collectivités territoriales ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$orga_etat2 <- renderInfoBox(
        infoBox(
          icon = icon("university"),
          p("Estimez-vous avoir accès aux services publics dont vous avez besoin ?"),
          paste0( "Oui à ", city_chosen()%>%
                    filter(reference=="L'organisation de l'état")%>%
                    pull(`Estimez-vous avoir accès aux services publics dont vous avez besoin ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$orga_etat3 <- renderInfoBox(
        infoBox(
          icon = icon("wifi"),
          p("Avez-vous déjà utilisé certaines de ces nouvelles formes de services publics ? (sur internet)"),
          paste0( "Oui à ", city_chosen()%>%
                    filter(reference=="L'organisation de l'état")%>%
                    pull(`Avez-vous déjà utilisé certaines de ces nouvelles formes de services publics ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$orga_etat4 <- renderInfoBox(
        infoBox(
          icon = icon("thumbs-up"),
          p("Si oui, en avez-vous été satisfait ?"),
          paste0( "Oui à ", city_chosen()%>%
                    filter(reference=="L'organisation de l'état")%>%
                    pull(`Si oui, en avez-vous été satisfait ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$orga_etat5 <- renderInfoBox(
        infoBox(
          icon = icon("user-edit"),
          p("Connaissez-vous le \"droit à l'erreur\", c'est-à-dire le droit d'affirmer votre bonne foi lorsque vous faites un erreur dans vos déclarations ?"),
          paste0( "Oui à ", city_chosen()%>%
                    filter(reference=="L'organisation de l'état")%>%
                    pull(`Connaissez-vous le \"droit à l'erreur\", c'est-à-dire le droit d'affirmer votre bonne foi lorsque vous faites un erreur dans vos déclarations ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$orga_etat6 <- renderInfoBox(
        infoBox(
          icon = icon("user-times"),
          p("Si oui, avez-vous déjà utilisé ce droit à l'erreur ?"),
          paste0( "Oui à ", city_chosen()%>%
                    filter(reference=="L'organisation de l'état")%>%
                    pull(`Si oui, avez-vous déjà utilisé ce droit à l'erreur ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$orga_etat7 <- renderInfoBox(
        infoBox(
          icon = icon("chalkboard-teacher"),
          p("Faut-il donner plus d'autonomie aux fonctionnaires de terrain ?"),
          paste0( "Oui à ", city_chosen()%>%
                    filter(reference=="L'organisation de l'état")%>%
                    pull(`Faut-il donner plus d'autonomie aux fonctionnaires de terrain ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$orga_etat8 <- renderInfoBox(
        infoBox(
          icon = icon("building"),
          p("Faut-il revoir le fonctionnement et la formation de l'administration ?"),
          paste0( "Oui à ", city_chosen()%>%
                    filter(reference=="L'organisation de l'état")%>%
                    pull(`Faut-il revoir le fonctionnement et la formation de l'administration ?`)%>%
                    mean(na.rm = TRUE)%>%
                    round(digits = 3)%>%
                    prod(100), "% ")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$nb_contributions_fiscalite <- renderValueBox(
        valueBox(
          p("Nb contributions"), paste0(city_chosen()%>% filter(reference=="La fiscalité et les dépenses publiques")%>% count()),
          color = "navy"
        )
      )
      
      incProgress(amount = 1/24)
      
    })
  }
  )
}  


# Run the application 
shinyApp(ui = ui, server = server)



