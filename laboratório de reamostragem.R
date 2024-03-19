library(ordinal)
library(tidyverse)
library(caret)

library(rattle)



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
  mutate(mesma_regiao = ifelse(a01ra==i08,1,0)) %>%
  select(a01ra, idade, e04, e06, e07, e05, i08, i10 ,i09_8,  renda_ind_r, peso_mor, mesma_regiao ) #I10 é a variável independente


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


dados_teste_chi_quadrado<-
  amostra_expandida_1400 %>%
  filter(!i10%in%c(88888,99999))

teste_tempo<-
chisq.test(as.factor(dados_teste_chi_quadrado$a01ra), as.factor(dados_teste_chi_quadrado$i10), simulate.p.value = TRUE)

teste_destino<-
chisq.test(as.factor(dados_teste_chi_quadrado$a01ra), as.factor(dados_teste_chi_quadrado$i08), simulate.p.value = TRUE)


teste_cor<-
  chisq.test(as.factor(dados_teste_chi_quadrado$e06), as.factor(dados_teste_chi_quadrado$i10), simulate.p.value = TRUE)


teste_meio_transporte<-
  chisq.test(as.factor(dados_teste_chi_quadrado$i09_8), as.factor(dados_teste_chi_quadrado$i10), simulate.p.value = TRUE)


teste_mesma_regiao<-
  chisq.test(as.factor(dados_teste_chi_quadrado$mesma_regiao), as.factor(dados_teste_chi_quadrado$i10), simulate.p.value = TRUE)

teste_sexo<-
  chisq.test(as.factor(dados_teste_chi_quadrado$e04), as.factor(dados_teste_chi_quadrado$i10), simulate.p.value = TRUE)



gera_modelo <- function(.data, seed=1972, pesos=FALSE, a_prop){
  
  set.seed(seed)
  dados_treino<- 
    .data %>%
    filter(!(i10%in%c('88888','99999')),
           !is.na(renda_ind_r)) %>%
    slice_sample(prop = a_prop)
  
  if (pesos){
    pesos_parm = dados_treino$peso_mor
  } else{
    pesos_parm = NULL
  }

  dados_treino<-
  dados_treino %>%
    mutate(mesma_regiao = ifelse(a01ra==i08,1,0),
           tempos_deslocamento = ifelse(i10<=2,"Até 30 minutos","Acima de 30 minutos"))  
  
  dados_treino$i10 <- as.factor(dados_treino$i10)
  dados_treino$e04 <- as.factor(dados_treino$e04)
  dados_treino$e06 <- as.factor(dados_treino$e06)
  dados_treino$e07 <- as.factor(dados_treino$e07)
  dados_treino$e05 <- as.factor(dados_treino$e05)
  dados_treino$i08 <- as.factor(dados_treino$i08)
  dados_treino$a01ra <- as.factor(dados_treino$a01ra)
  dados_treino$i09_8 <- as.factor(dados_treino$i09_8)
  dados_treino$mesma_regiao <- as.factor(dados_treino$mesma_regiao)
  
  
  # Create model with ramdom parameters
  control_dt <- trainControl(method="cv")
  seed <- 1972
  set.seed(seed)
  
  

  dt_model <- train(tempos_deslocamento ~  renda_ind_r+ a01ra +i09_8 + i08  +e04 ,
                    data=dados_treino, 
                    method="rpart", 
                    weights = pesos_parm,  
                    trControl=control_dt)
  
  dt_model

}


modelo_amostra<- gera_modelo(amostra_expandida_1400, a_prop = 0.7)

fancyRpartPlot(modelo_amostra$finalModel)



modelo_microdados_70<- gera_modelo(PDAD_2021_Moradores, pesos = TRUE,  a_prop = 0.7)

fancyRpartPlot(modelo_microdados_70$finalModel)

