library(tidyverse)


(PDAD_2021_Moradores$idade[1] * PDAD_2021_Moradores$peso_mor[1])


media_idade<- mean(PDAD_2021_Moradores$idade)

media_idade_ponderada <- weighted.mean(PDAD_2021_Moradores$idade, PDAD_2021_Moradores$peso_mor)

media_idade

media_idade_ponderada



media_remuneracao<- mean(PDAD_2021_Moradores$i20[!(PDAD_2021_Moradores$i20 %in% c(77777, 88888, 99999 ))])
media_remuneracao_ponderada <- weighted.mean(PDAD_2021_Moradores$i20[!(PDAD_2021_Moradores$i20 %in% c(77777, 88888, 99999 ))], 
                                             PDAD_2021_Moradores$peso_mor[!(PDAD_2021_Moradores$i20 %in% c(77777, 88888, 99999 ))])

media_remuneracao
media_remuneracao_ponderada


names(PDAD_2021_Moradores)


soma_pesos<- sum(PDAD_2021_Moradores$peso_mor)


perc_pop_ra<-
  PDAD_2021_Moradores|>
  summarise( perc_pop = (sum(peso_mor) /soma_pesos)*100 ,
             .by=c(a01ra)) %>%
  arrange(a01ra)



perc_pop_ra_s_peso<-
  PDAD_2021_Moradores|>
  summarise( perc_pop = (n()/NROW(PDAD_2021_Moradores))*100 ,
             .by=c(a01ra)) %>%
  arrange(a01ra)



require(maptools)
require(rgdal)
require(RcColorBrewer)
require(plotrix)
require(classInt)
library(GISTools)


library(sf)


sf::read_sf()

shape <- sf::read_sf(dsn = "ras_pdad_2013.shp")

st_drivers(what = "vector")

mapa <- readShapeSpatial('C:\\Users\\kraise\\Desktop\\Meus programas\\R programas\\ras_pdad_2013.shp')
View(mapa)













