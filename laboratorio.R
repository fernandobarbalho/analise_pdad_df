unique(PDAD_2021_Moradores$PESO_MOR)

(PDAD_2021_Moradores$idade[1] * PDAD_2021_Moradores$PESO_MOR[1])


media_idade<- mean(PDAD_2021_Moradores$idade)

media_idade_ponderada <- weighted.mean(PDAD_2021_Moradores$idade, PDAD_2021_Moradores$PESO_MOR)

media_idade

media_idade_ponderada


PDAD_2021_Moradores$I20[1]


media_remuneracao<- mean(PDAD_2021_Moradores$I20[!(PDAD_2021_Moradores$I20 %in% c(77777, 88888, 99999 ))])
media_remuneracao_ponderada <- weighted.mean(PDAD_2021_Moradores$I20[!(PDAD_2021_Moradores$I20 %in% c(77777, 88888, 99999 ))], 
                                             PDAD_2021_Moradores$PESO_MOR[!(PDAD_2021_Moradores$I20 %in% c(77777, 88888, 99999 ))])

media_remuneracao
media_remuneracao_ponderada
