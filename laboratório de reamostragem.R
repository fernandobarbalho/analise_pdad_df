library(ordinal)

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


amostra_expandida_1400$i10 <- factor(amostra_expandida_1400$i10, ordered = TRUE)

modelo <- clm(i10 ~ idade + as.factor(e04) + as.factor(e06) +as.factor(e07) + as.factor(e05) + as.factor(i08) + renda_ind_r,
              data = amostra_expandida_1400)


modelo<- clm(as.factor(i10) ~ idade + as.factor(e04) + as.factor(e06) +as.factor(e07) + as.factor(e05) + as.factor(i08) + renda_ind_r,
              weights = peso_mor, data = PDAD_2021_Moradores)


library(rpart)
library(rpart.plot)



amostra_expandida_1400$i10 <- as.factor(amostra_expandida_1400$i10)
amostra_expandida_1400$e04 <- as.factor(amostra_expandida_1400$e04)
amostra_expandida_1400$e06 <- as.factor(amostra_expandida_1400$e06)
amostra_expandida_1400$e07 <- as.factor(amostra_expandida_1400$e07)
amostra_expandida_1400$e05 <- as.factor(amostra_expandida_1400$e05)
amostra_expandida_1400$i08 <- as.factor(amostra_expandida_1400$i08)

dados_treino<- 
  amostra_expandida_1400 %>%
  slice_sample(prop = 0.1)


modelo_arvore <- rpart(as.factor(i10) ~ idade + as.factor(e04) + as.factor(e06) + as.factor(e07) + as.factor(e05) + as.factor(i08) + renda_ind_r,
                       data = dados_treino,
                       method = "class",
                       control = rpart.control(maxdepth = 4, minsplit = 20, cp = 0.01, minbucket = 10))


modelo_arvore_renda <- rpart(as.factor(i10) ~  renda_ind_r,
                       data = dados_treino)

rpart.plot(modelo_arvore_renda)


