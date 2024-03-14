#laboratório de reamostragem

 replicate(3,amostra_trabalho[1,])
 
 bind_rows(replicate(3, amostra_trabalho[1,], simplify = FALSE))

set.seed(1972)
amostra_trabalho_400<-
  PDAD_2021_Moradores%>%
  slice_sample(n=400) %>%
  select(a01ra, idade, e04, e06, e07, e05, e08, i10 ,  renda_ind_r, peso_mor ) #I10 é a variável independente


#Construção da amostra expandida de acordo com o peso
amostra_expandida_400<-
  purrr::map_dfr(1:NROW(amostra_trabalho_400) ,function(linha){
    #print(linha)
    bind_rows(replicate(round(amostra_trabalho_400$peso_mor[linha],0), 
                        amostra_trabalho_400[linha,], 
                        simplify = FALSE))
  }) 


amostra_expandida_400 %>%
  summarise( prop_populacao =  (n()/NROW(amostra_expandida_400))*100,
             .by = a01ra) %>%
  arrange(desc(prop_populacao))


set.seed(1972)
amostra_trabalho_800<-
  PDAD_2021_Moradores%>%
  slice_sample(n=800) %>%
  select(a01ra, idade, e04, e06, e07, e05, e08, i10 ,  renda_ind_r, peso_mor ) #I10 é a variável independente


#Construção da amostra expandida de acordo com o peso
amostra_expandida_800<-
  purrr::map_dfr(1:NROW(amostra_trabalho_800) ,function(linha){
    #print(linha)
    bind_rows(replicate(round(amostra_trabalho_800$peso_mor[linha],0), 
                        amostra_trabalho_800[linha,], 
                        simplify = FALSE))
  }) 

amostra_expandida_800 %>%
  summarise( prop_populacao =  (n()/NROW(amostra_expandida_800))*100,
             .by = a01ra) %>%
  arrange(desc(prop_populacao))



set.seed(1972)
amostra_trabalho_1000<-
  PDAD_2021_Moradores%>%
  slice_sample(n=1000) %>%
  select(a01ra, idade, e04, e06, e07, e05, e08, i10 ,  renda_ind_r, peso_mor ) #I10 é a variável independente


#Construção da amostra expandida de acordo com o peso
amostra_expandida_1000<-
  purrr::map_dfr(1:NROW(amostra_trabalho_1000) ,function(linha){
    #print(linha)
    bind_rows(replicate(round(amostra_trabalho_1000$peso_mor[linha],0), 
                        amostra_trabalho_1000[linha,], 
                        simplify = FALSE))
  }) 

amostra_expandida_1000 %>%
  summarise( prop_populacao =  (n()/NROW(amostra_expandida_1000))*100,
             .by = a01ra) %>%
  arrange(desc(prop_populacao))


set.seed(13)
amostra_trabalho_1000<-
  PDAD_2021_Moradores%>%
  slice_sample(n=1000) %>%
  select(a01ra, idade, e04, e06, e07, e05, e08, i10 ,  renda_ind_r, peso_mor ) #I10 é a variável independente


#Construção da amostra expandida de acordo com o peso
amostra_expandida_1000<-
  purrr::map_dfr(1:NROW(amostra_trabalho_1000) ,function(linha){
    #print(linha)
    bind_rows(replicate(round(amostra_trabalho_1000$peso_mor[linha],0), 
                        amostra_trabalho_1000[linha,], 
                        simplify = FALSE))
  }) 

amostra_expandida_1000 %>%
  summarise( prop_populacao =  (n()/NROW(amostra_expandida_1000))*100,
             .by = a01ra) %>%
  arrange(desc(prop_populacao))


set.seed(10)
amostra_trabalho_1000<-
  PDAD_2021_Moradores%>%
  slice_sample(n=1000) %>%
  select(a01ra, idade, e04, e06, e07, e05, e08, i10 ,  renda_ind_r, peso_mor ) #I10 é a variável independente


#Construção da amostra expandida de acordo com o peso
amostra_expandida_1000<-
  purrr::map_dfr(1:NROW(amostra_trabalho_1000) ,function(linha){
    #print(linha)
    bind_rows(replicate(round(amostra_trabalho_1000$peso_mor[linha],0), 
                        amostra_trabalho_1000[linha,], 
                        simplify = FALSE))
  }) 

amostra_expandida_1000 %>%
  summarise( prop_populacao =  (n()/NROW(amostra_expandida_1000))*100,
             .by = a01ra) %>%
  arrange(desc(prop_populacao))


set.seed(1)
amostra_trabalho_1200<-
  PDAD_2021_Moradores%>%
  slice_sample(n=1200) %>%
  select(a01ra, idade, e04, e06, e07, e05, e08, i10 ,  renda_ind_r, peso_mor ) #I10 é a variável independente


#Construção da amostra expandida de acordo com o peso
amostra_expandida_1200<-
  purrr::map_dfr(1:NROW(amostra_trabalho_1200) ,function(linha){
    #print(linha)
    bind_rows(replicate(round(amostra_trabalho_1200$peso_mor[linha],0), 
                        amostra_trabalho_1200[linha,], 
                        simplify = FALSE))
  }) 

amostra_expandida_1200 %>%
  summarise( prop_populacao =  (n()/NROW(amostra_expandida_1200))*100,
             .by = a01ra) %>%
  arrange(desc(prop_populacao))


set.seed(2)
amostra_trabalho_1400<-
  PDAD_2021_Moradores%>%
  slice_sample(n=1400) %>%
  select(a01ra, idade, e04, e06, e07, e05, i08, i10 ,  renda_ind_r, peso_mor ) #I10 é a variável independente


#Construção da amostra expandida de acordo com o peso
amostra_expandida_1400<-
  purrr::map_dfr(1:NROW(amostra_trabalho_1400) ,function(linha){
    #print(linha)
    bind_rows(replicate(round(amostra_trabalho_1400$peso_mor[linha],0), 
                        amostra_trabalho_1400[linha,], 
                        simplify = FALSE))
  }) 

amostra_expandida_1400 %>%
  summarise( prop_populacao =  (n()/NROW(amostra_expandida_1400))*100,
             .by = a01ra) %>%
  arrange(desc(prop_populacao))


teste_tempo<-
chisq.test(as.factor(amostra_expandida_1400$a01ra), as.factor(amostra_expandida_1400$i10), simulate.p.value = TRUE)

teste_destino<-
chisq.test(as.factor(amostra_expandida_1400$a01ra), as.factor(amostra_expandida_1400$i08), simulate.p.value = TRUE)
