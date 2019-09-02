library(readr)
library(tidyverse)
library(stringr)
library(forcats)



#load all 4 DF
democratie_citoyennete_df<-read_csv("DEMOCRATIE_ET_CITOYENNETE.csv", locale = locale("fr"))
organisation_eta_df <- read_csv("ORGANISATION_DE_LETAT_ET_DES_SERVICES_PUBLICS.csv", locale = locale("fr"))
fiscalite_df <- read_csv("LA_FISCALITE_ET_LES_DEPENSES_PUBLIQUES.csv", locale = locale("fr"))
transition_ecologique_df<- read_csv("LA_TRANSITION_ECOLOGIQUE.csv", locale = locale("fr"))


#cleaning column title in each df and filter using usable data
democratie_citoyennete_df_filtered<- democratie_citoyennete_df%>%select(2,3,4,10,11,13,16,19,22,25,29,42)%>% filter(nchar(authorZipCode) == 5 )
transition_ecologique_df_filtered<- transition_ecologique_df%>%select(2,3,4,10,11,14,16,20,22)%>% filter(nchar(authorZipCode) == 5 )
fiscalite_df_filtered <- fiscalite_df%>% select(2,3,4,10,11)%>% filter(nchar(authorZipCode) == 5 )
organisation_eta_df_filtered <- organisation_eta_df%>% select(2,3,4,10,11,13,15,18,19,23,24,27,29)%>% filter(nchar(authorZipCode) == 5 )


cleaning_title<- function (x){
names(x)<-str_replace(names(x), pattern = "^.*[:blank:]-[:blank:]", "")
x
}

big_df<-list(democratie_citoyennete_df_filtered, transition_ecologique_df_filtered, fiscalite_df_filtered, organisation_eta_df_filtered)

big_df <- map_dfr(big_df, ~cleaning_title(.x))

names(big_df[11])<- "Faut-il les transformer ? (nos assemblées, dont le Sénat, conseil économique, social et environnemental)"


#recoding factor variables

recode_reference <- function(x){x$reference<-  case_when( str_detect(x$reference, pattern = "^1") ~ "Démocratie et citoyenneté",
                                                                 str_detect(x$reference, "^2") ~ "La transition écologique",
                                                                 str_detect(x$reference, "^3") ~ "La fiscalité et les dépenses publiques",
                                                                 str_detect(x$reference, "^4") ~ "L'organisation de l'état")
                                                                return(x)}

big_df <- recode_reference(big_df)



#import info on cities
codepostaux <- read_csv2("laposte.csv", col_types = "_cf__c")

#cleaning file so each zip code = 1 city name + adding city name
codepostaux[, c("lat", "long")] <-str_split_fixed(codepostaux$coordonnees_gps, pattern = ", ", n =2)

adding_city_name <- function (x) {
  x$Nom_commune <- case_when ( x$authorZipCode == 75000 ~ "PARIS",
                               x$authorZipCode == 69000 ~ "LYON",
                               x$authorZipCode == 13000 ~ "MARSEILLE",
                               x$authorZipCode == 98713  ~ "PAPEETE",
                               x$authorZipCode == 99000  ~ "MONACO",
                               TRUE ~ x$Nom_commune)
  
  x$lat<- case_when ( x$authorZipCode == 75000 ~ 48.856614,
                      x$authorZipCode == 69000 ~ 45.764043,
                      x$authorZipCode == 13000 ~ 43.296482,
                      x$authorZipCode == 98713  ~ -17.535000,
                      x$authorZipCode == 99000  ~ 43.738418,
                      TRUE ~ x$lat)
  
  x$long<-case_when ( x$authorZipCode == 75000 ~ 2.3522219,
                      x$authorZipCode == 69000 ~ 4.835659,
                      x$authorZipCode == 13000 ~ 5.369780,
                      x$authorZipCode == 98713  ~ -149.569600,
                      x$authorZipCode == 99000  ~ 7.424616,
                      TRUE ~ x$long)
  
  return(x)
}


codepostaux<- codepostaux %>% 
  map_at(.at = c("lat", "long"), .f = as.numeric)%>%
  as_tibble()%>%
  group_by(Code_postal) %>% summarize(Nom_commune = paste(Nom_commune, collapse = ", "),
                                                    lat = mean(lat),
                                                    long = mean(long))



#joining info and DF
big_df<- big_df%>%
  left_join( codepostaux, by =c ("authorZipCode" = "Code_postal"))


big_df <- big_df %>%
                  adding_city_name()

big_df[c(6:24)] <- map_dfr(big_df[c(6:24)], str_replace, pattern = "Oui|Je n'utilise pas la voiture pour des déplacements quotidiens|Une bonne chose" , "TRUE" )%>%
 map_dfr(str_replace, pattern = "Non|Une mauvaise chose", "FALSE" )%>%
 map_dfr(str_replace, pattern = "Je ne sais pas", "NA" )%>%
map_dfr(as.logical)

#cleaning weird things happening with Marseille

big_df$Nom_commune[str_which(string = grand_debat$Nom_commune, pattern ="^MARSEILLE[:blank:]13")]<- "MARSEILLE 13"
big_df$Nom_commune[str_which(string = grand_debat$Nom_commune, pattern ="^MARSEILLE[:blank:]08")]<- "MARSEILLE 08"
big_df$Nom_commune[str_which(string = grand_debat$Nom_commune, pattern ="^MARSEILLE[:blank:]11")]<- "MARSEILLE 11"
big_df$Nom_commune[str_which(string = grand_debat$Nom_commune, pattern ="^MARSEILLE[:blank:]09")]<- "MARSEILLE 09"
big_df$Nom_commune[str_which(string = grand_debat$Nom_commune, pattern ="^MARSEILLE[:blank:]16")]<- "MARSEILLE 16"

write_csv(big_df, "grand_debat.csv")

grand_debat <- read_csv(file = "grand_debat.csv")

