library(shiny)
library(tidyverse)
library(DT)
library(tools)
library(shinydashboard)
library(dashboardthemes)

grand_debat <- read_csv(file = "grand_debat.csv")


server <- function(input, output) {
  
  observeEvent(input$resultats,{
    withProgress(message ="Calcul des données" , min =0, max = 1, value = 0, {
      
      city_chosen<- reactive({
        isolate({
          grand_debat_filtered<- tibble()
            if(is_empty(input$select_city_name)){
              grand_debat_filtered <- grand_debat%>% filter(authorZipCode %in% input$select_city_zip_code | Nom_commune %in% input$select_city_name)
            }else if("France entière" %in% input$select_city_name){
            grand_debat_filtered <- grand_debat} else if ("PARIS" %in% input$select_city_name | "LYON" %in% input$select_city_name | "MARSEILLE" %in% input$select_city_name ){
              if("PARIS" %in% input$select_city_name){grand_debat_filtered<- bind_rows(grand_debat[str_which(string = grand_debat$Nom_commune, pattern ="^PARIS(?!OT)"),], grand_debat_filtered) }
              if("LYON" %in% input$select_city_name){grand_debat_filtered<- bind_rows(grand_debat[str_which(string = grand_debat$Nom_commune, pattern ="^LYON"),], grand_debat_filtered) }
              if("MARSEILLE" %in% input$select_city_name){grand_debat_filtered<- bind_rows(grand_debat[str_which(string = grand_debat$Nom_commune, pattern ="^MARSEILLE"),],grand_debat_filtered) }
              grand_debat_filtered <- bind_rows(grand_debat%>% filter(authorZipCode %in% input$select_city_zip_code | Nom_commune %in% input$select_city_name),grand_debat_filtered)
            }else{
            grand_debat_filtered <- grand_debat%>% filter(authorZipCode %in% input$select_city_zip_code | Nom_commune %in% input$select_city_name)
            }
          return(grand_debat_filtered)
        }) })
      
contribution <- function(theme){city_chosen() %>% filter(reference== theme)%>% count()}
question <- function (theme, name, type = "oui_non"){
  result<- city_chosen() %>% filter(reference== theme)%>%
    pull(name)%>%
    mean(na.rm = TRUE)%>%
    round(digits = 3)%>%
    prod(100)
  if(type== "oui_non"){
  if(result >= 50){paste0("Oui à ", result, "%")
  }else{paste0("Non à ", 100 - result, "%")}}
  else if(type == "bonne_mauvaise" ){
    if(result >= 50){paste0("A ", result, "% une bonne chose")
    }else{paste0("A ", 100 - result, "% une mauvaise chose")}
  }
}

      incProgress(amount = 1/24)
      
      output$nb_contributions_democratie <- renderValueBox(
        valueBox(
          p("Nb contributions"), paste0(contribution("Démocratie et citoyenneté")),
          color = "red"
        )
      )
      
      
      incProgress(amount = 1/24)
      
      output$democratie1 <- renderInfoBox(
        infoBox(
          icon = icon("briefcase"),
          p("En dehors des élus politiques, faut-il donner un rôle plus important aux associations et aux organisations syndicales et professionnelles ?"),
          question("Démocratie et citoyenneté","En dehors des élus politiques, faut-il donner un rôle plus important aux associations et aux organisations syndicales et professionnelles ?" )
        )
      )
      
      incProgress(amount = 1/24)
      
      output$democratie2 <- renderInfoBox(
        infoBox(
          icon = icon("user-times"),
          p("Le non-cumul des mandats instauré en 2017 pour les parlementaires (députés et sénateurs) est :"),
          question("Démocratie et citoyenneté", "Le non-cumul des mandats instauré en 2017 pour les parlementaires (députés et sénateurs) est :", type = "bonne_mauvaise") 
        )
      ) 
      
      incProgress(amount = 1/24)
      
      output$democratie3 <- renderInfoBox(
        infoBox(
          icon = icon("male"),
          p("Pensez-vous qu'il serait souhaitable de réduire le nombre d'élus (hors députés et sénateurs)"),
          question("Démocratie et citoyenneté","Pensez-vous qu'il serait souhaitable de réduire le nombre d'élus (hors députés et sénateurs) ?")
        )
      ) 
      
      incProgress(amount = 1/24)
      
      output$democratie4 <- renderInfoBox(
        infoBox(
          icon = icon("personbooth"),
          p("Faut-il prendre en compte le vote blanc ?"),
          question("Démocratie et citoyenneté","Faut-il prendre en compte le vote blanc ?")
        )
      )
      incProgress(amount = 1/24)
      
      output$democratie5 <- renderInfoBox(
        infoBox(
          icon = icon("bullhorn"),
          p("Faut-il faciliter le déclenchement du référendum d'initiative partagée (le RIP est organisé à l'initiative de membres du Parlement soutenu par une partie du corps électoral) qui est applicable depuis 2015 ?"),
          question("Démocratie et citoyenneté", "Faut-il faciliter le déclenchement du référendum d'initiative partagée (le RIP est organisé à l'initiative de membres du Parlement soutenu par une partie du corps électoral) qui est applicable depuis 2015 ?")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$democratie6 <- renderInfoBox(
        infoBox(
          icon = icon("landmark"),
          p("Faut-il  transformer nos assemblées, dont le Sénat, conseil économique, social et environnemental?"),
          question("Démocratie et citoyenneté", "Faut-il les transformer ?")
        )
      )
      
      incProgress(amount = 1/21)
      
      output$democratie7 <- renderInfoBox(
        infoBox(
          icon = icon("hand-holding-usd"),
          p("Pensez-vous qu'il faille instaurer des contreparties aux différentes allocations de solidarité ?"),
          question("Démocratie et citoyenneté","Pensez-vous qu'il faille instaurer des contreparties aux différentes allocations de solidarité ?" )
        )
      )
      
      incProgress(amount = 1/24)
      
      output$nb_contributions_transition_ecologique <- renderValueBox(
        valueBox(
          p("Nb contributions"), paste0(contribution("La transition écologique")),
          color = "olive"
        )
      )
      
      incProgress(amount = 1/24)
      
      output$ecologie1 <- renderInfoBox(
        infoBox(
          icon = icon("thermometer"),
          p("Diriez-vous que votre vie quotidienne est aujourd'hui touchée par le changement climatique ?"),
          question("La transition écologique","Diriez-vous que votre vie quotidienne est aujourd'hui touchée par le changement climatique ?" )
        )
      )
      
      incProgress(amount = 1/24)
      
      output$ecologie2 <- renderInfoBox(
        infoBox(
          icon = icon("leaf"),
          p("À titre personnel, pensez-vous pouvoir contribuer à protéger l'environnement ?"),
          question("La transition écologique", "À titre personnel, pensez-vous pouvoir contribuer à protéger l'environnement ?" )
        )
      )
      
      incProgress(amount = 1/24)
      
      output$ecologie3 <- renderInfoBox(
        infoBox(
          icon = icon("fire"),
          p("Par rapport à votre mode de chauffage actuel, pensez-vous qu'il existe des solutions alternatives plus écologiques ?"),
          question("La transition écologique", "Par rapport à votre mode de chauffage actuel, pensez-vous qu'il existe des solutions alternatives plus écologiques ?" )
        )
      )
      
      incProgress(amount = 1/24)
      
      output$ecologie4 <- renderInfoBox(
        infoBox(
          icon = icon("bicycle"),
          p("Avez-vous pour vos déplacements quotidiens la possibilité de recourir à des solutions de mobilité alternatives à la voiture individuelle comme les transports en commun, le covoiturage, l'auto-partage, le transport à la demande, le vélo, etc. ?"),
          question("La transition écologique", "Avez-vous pour vos déplacements quotidiens la possibilité de recourir à des solutions de mobilité alternatives à la voiture individuelle comme les transports en commun, le covoiturage, l'auto-partage, le transport à la demande, le vélo, etc. ?")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$nb_contributions_orga_eta <- renderValueBox(
        valueBox(
          p("Nb contributions"), paste0(contribution("L'organisation de l'état")),
          color = "yellow"
        )
      )
      
      incProgress(amount = 1/24)
      
      output$orga_etat1 <- renderInfoBox(
        infoBox(
          icon = icon("city"),
          p("Selon vous, l'Etat doit-il aujourd'hui transférer de nouvelles missions aux collectivités territoriales ?"),
          question("L'organisation de l'état","Selon vous, l'Etat doit-il aujourd'hui transférer de nouvelles missions aux collectivités territoriales ?" )
        )
      )
      
      incProgress(amount = 1/24)
      
      output$orga_etat2 <- renderInfoBox(
        infoBox(
          icon = icon("university"),
          p("Estimez-vous avoir accès aux services publics dont vous avez besoin ?"),
          question("L'organisation de l'état", "Estimez-vous avoir accès aux services publics dont vous avez besoin ?")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$orga_etat3 <- renderInfoBox(
        infoBox(
          icon = icon("wifi"),
          p("Avez-vous déjà utilisé certaines de ces nouvelles formes de services publics ? (sur internet)"),
          question("L'organisation de l'état", "Avez-vous déjà utilisé certaines de ces nouvelles formes de services publics ?")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$orga_etat4 <- renderInfoBox(
        infoBox(
          icon = icon("thumbs-up"),
          p("Si oui, en avez-vous été satisfait ?"),
          question("L'organisation de l'état", "Si oui, en avez-vous été satisfait ?")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$orga_etat5 <- renderInfoBox(
        infoBox(
          icon = icon("user-edit"),
          p("Connaissez-vous le \"droit à l'erreur\", c'est-à-dire le droit d'affirmer votre bonne foi lorsque vous faites un erreur dans vos déclarations ?"),
          question("L'organisation de l'état", "Connaissez-vous le \"droit à l'erreur\", c'est-à-dire le droit d'affirmer votre bonne foi lorsque vous faites un erreur dans vos déclarations ?")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$orga_etat6 <- renderInfoBox(
        infoBox(
          icon = icon("user-times"),
          p("Si oui, avez-vous déjà utilisé ce droit à l'erreur ?"),
          question("L'organisation de l'état", "Si oui, avez-vous déjà utilisé ce droit à l'erreur ?")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$orga_etat7 <- renderInfoBox(
        infoBox(
          icon = icon("chalkboard-teacher"),
          p("Faut-il donner plus d'autonomie aux fonctionnaires de terrain ?"),
          question("L'organisation de l'état", "Faut-il donner plus d'autonomie aux fonctionnaires de terrain ?")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$orga_etat8 <- renderInfoBox(
        infoBox(
          icon = icon("building"),
          p("Faut-il revoir le fonctionnement et la formation de l'administration ?"),
          question("L'organisation de l'état", "Faut-il revoir le fonctionnement et la formation de l'administration ?")
        )
      )
      
      incProgress(amount = 1/24)
      
      output$nb_contributions_fiscalite <- renderValueBox(
        valueBox(
          p("Nb contributions"), paste0(contribution("La fiscalité et les dépenses publiques")),
          color = "navy"
        )
      )
      
      incProgress(amount = 1/24)
      
    })
  }
  )
}  




