library(rvest)
library(tidyverse)

link="https://duproprio.com/fr/quebec-rive-sud/levis/maison-a-vendre?pageNumber=2"
page=read_html(link)


adresse<- page %>% html_nodes(".search-results-listings-list__item-description__address") %>% html_text()
adresse<-str_replace_all(adresse, "\n","")

metadata_aire_terrain <- page %>% html_nodes(".search-results-listings-list__item-description__characteristics") %>% html_text() %>%
                 str_split("\n", simplify = TRUE)
aireHabitable <- metadata_aire_terrain[,13]
aireHabitable <- parse_number(str_extract(aireHabitable, "\\d+(?:[.,\\s]\\d{0,3})?"), locale = locale(decimal_mark = ".", grouping_mark = " "))


metadata_aire_terrain <- page %>% html_nodes(".search-results-listings-list__item-description__characteristics") %>% html_text() %>%
                         str_split("\n", simplify = TRUE)
taille_terrain <- metadata_aire_terrain[,17]
taille_terrain <- parse_number(str_extract(taille_terrain , "\\d+(?:[.,\\s]\\d{0,3})?"), locale = locale(decimal_mark = ".", grouping_mark = " "))

prix <- page %>% html_nodes(".search-results-listings-list__item-description__price h2") %>% html_text()
prix <- extract_numeric(prix)


region <- page %>% html_nodes(".search-results-listings-list__item-description__city span") %>% html_text()

nombre_chambre<- page %>% html_nodes(".search-results-listings-list__item-description__characteristics__item:nth-child(1)") %>% html_text() %>%
                 str_split("\n", simplify = TRUE)
nombre_chambre<- parse_number(nombre_chambre[,3]) 


salle_bain<- page %>% html_nodes(".search-results-listings-list__item-description__characteristics__item:nth-child(2)") %>% html_text()%>%
             str_split("\n", simplify = TRUE)

#replace_na(): replace NA value in a column x and y by 0
nbre_salleBain <- tibble(x=parse_number(salle_bain[,3]),y=parse_number(salle_bain[,4])) %>% replace_na(list(x=0,y=0)) %>%mutate(nbr=x+y)%>% select(nbr)

df <- data.frame(Prix=prix, Aire_habitable=aireHabitable, Taille_terrain=taille_terrain, Nombre_chambre=nombre_chambre, Nombre_salleBain=nbre_salleBain, Adresse=adresse, Region=region)




