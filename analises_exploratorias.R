library(tidyverse)
library(geobr)


#Análise com tabela pré-formatada de dados de deslocamentos para escola 

deslocamentos_escola %>%
  select(-Total)%>%
  rename(local = Local) %>%
  mutate_at(vars(-local), ~ifelse(. == "(***)", NA, .)) %>%
  mutate_at(vars(-local), ~as.numeric(.)) %>%
  pivot_longer(2:10, names_to = "tipo_deslocamento", values_to = "valor") %>%
  filter(local == "DF" ) %>%
  mutate(tipo_deslocamento = reorder(tipo_deslocamento, valor)) %>%
  ggplot(aes(x=valor, y=tipo_deslocamento )) +
  geom_col(fill="white")+
  geom_text(aes(label = paste0(round(valor, 1),"%")), 
            hjust = c(rep(1.2,5),rep(-0.1,4)), 
            size = 3, 
            color = c(rep("black",5),rep("white",4))) +
  theme_light() +
  theme(
    panel.background = element_rect(fill="black"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Principal meio de transporte para escola no DF",
    subtitle = "Dados de 2021",
    x= "",
    y=""
  )


dados_grafico<-
  deslocamentos_escola %>%
  select(-Total)%>%
  rename(local = Local) %>%
  mutate_at(vars(-local), ~ifelse(. == "(***)", NA, .)) %>%
  mutate_at(vars(-local), ~as.numeric(.)) %>%
  pivot_longer(2:10, names_to = "tipo_deslocamento", values_to = "valor") %>%
  filter(tipo_deslocamento == "A pé") %>%
  filter(!is.na(valor))


cor<- ifelse(dados_grafico$local=="DF","red","white")

dados_grafico%>%
  mutate(local = reorder(local, valor)) %>%
  ggplot(aes(x=valor, y=local )) +
  geom_col(fill=cor)+
  geom_text(aes(label = paste0(round(valor, 1),"%")),
            hjust = 1,# c(rep(1.2,5),rep(-0.1,4)),
            vjust=+0.3,
            size = 3,
            color = "black")+ #c(rep("black",5),rep("white",4))) +
  theme_light() +
  theme(
    panel.background = element_rect(fill="black"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Deslocamentos a pé para escola no DF",
    subtitle = "Dados de 2021",
    x= "",
    y=""
  )

dados_grafico <-
  deslocamentos_escola %>%
  select(-Total)%>%
  rename(local = Local) %>%
  mutate_at(vars(-local), ~ifelse(. == "(***)", NA, .)) %>%
  mutate_at(vars(-local), ~as.numeric(.)) %>%
  pivot_longer(2:10, names_to = "tipo_deslocamento", values_to = "valor") %>%
  filter(tipo_deslocamento == "Automóvel") %>%
  filter(!is.na(valor))


cor<- ifelse(dados_grafico$local=="DF","red","white")

dados_grafico%>%
  mutate(local = reorder(local, valor)) %>%
  ggplot(aes(x=valor, y=local )) +
  geom_col(fill=cor)+
  geom_text(aes(label = paste0(round(valor, 1),"%")),
            hjust = 1,# c(rep(1.2,5),rep(-0.1,4)),
            vjust=+0.3,
            size = 3,
            color = "black")+ #c(rep("black",5),rep("white",4))) +
  theme_light() +
  theme(
    panel.background = element_rect(fill="black"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Deslocamentos por automóvel para escola no DF",
    subtitle = "Dados de 2021",
    x= "",
    y=""
  )

dados_grafico<-
  deslocamentos_escola %>%
  select(-Total)%>%
  rename(local = Local) %>%
  mutate_at(vars(-local), ~ifelse(. == "(***)", NA, .)) %>%
  mutate_at(vars(-local), ~as.numeric(.)) %>%
  pivot_longer(2:10, names_to = "tipo_deslocamento", values_to = "valor") %>%
  filter(tipo_deslocamento == "Ônibus") %>%
  filter(!is.na(valor)) %>%
  mutate(local = reorder(local, valor))

cor<- ifelse(dados_grafico$local=="DF","red","white")

dados_grafico %>%
  ggplot(aes(x=valor, y=local )) +
  geom_col(fill=cor)+
  geom_text(aes(label = paste0(round(valor, 1),"%")),
            hjust = 1,# c(rep(1.2,5),rep(-0.1,4)),
            vjust=+0.3,
            size = 3,
            color = "black")+ #c(rep("black",5),rep("white",4))) +
  theme_light() +
  theme(
    panel.background = element_rect(fill="black"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Deslocamentos por ônibus para escola no DF",
    subtitle = "Dados de 2021",
    x= "",
    y=""
  )


mapa_df<- geobr::read_neighborhood(year = 2010)

mapa_df<- geobr::read_metro_area()

#Análise com microdados

PDAD_2021_Moradores %>%
  filter(!(i20 %in% c(77777, 88888, 99999) )) %>%
  summarise( media_podenderada = weighted.mean(i20,peso_mor),
            .by = a01ra) %>%
  arrange(desc(media_podenderada)) %>%
  inner_join(nomes_regioes_administrativas)


tempo_por_ra<-
PDAD_2021_Moradores %>%
  filter(!(i10 %in% c( 88888, 99999) )) %>%
  summarise( quantidade = sum(peso_mor),
             .by = c(a01ra,i10) ) %>%
  arrange(desc(quantidade)) %>%
  inner_join(nomes_regioes_administrativas)


total_cor_raca<-
  PDAD_2021_Moradores%>%
  summarise(total = sum(peso_mor),
            .by = e06)


total_cor_raca_deslocamento<-
  PDAD_2021_Moradores%>%
  filter(!(i10 %in% c(99999) )) %>%
  summarise(total = sum(peso_mor),
            .by = e06)

tempo_por_cor_raca<-
PDAD_2021_Moradores %>%
  filter(!(i10 %in% c(99999) )) %>%
  summarise( quantidade = sum(peso_mor),
             .by = c(i10,e06) ) %>%
  ungroup() %>%
  inner_join(total_cor_raca_deslocamento) %>%
  mutate(proporcao = (quantidade / total)*100)
  arrange(desc(proporcao))

  

validos_amostra<-
  (amostra_expandida_1400 %>%
  filter(!i10%in%c(88888,99999)) %>%
  summarise(total = n()))$total


validos_microdados<-
  (PDAD_2021_Moradores %>%
     filter(!i10%in%c(88888,99999)) %>%
     summarise(total = n()))$total

    
amostra_expandida_1400 %>%
  filter(!i10%in%c(88888,99999)) %>%
  mutate(i10 =as.factor(i10)) %>%
  summarise(proporcao = (n()/validos_amostra)*100,
            .by = i10) %>%
  arrange(desc(proporcao))
  

PDAD_2021_Moradores %>%
  filter(!i10%in%c(88888,99999)) %>%
  mutate(i10 =as.factor(i10)) %>%
  summarise(proporcao = (n()/validos_microdados)*100,
            .by = i10) %>%
  arrange(desc(proporcao))

PDAD_2021_Moradores %>%
  summarise(populacao = sum(peso_mor),
            .by = a01ra) %>%
  mutate(a01ra = reorder(a01ra, populacao)) %>%
  ggplot(aes(x=populacao, y=a01ra)) +
  geom_col()
  


amostra_expandida_1400 %>%
  filter(!i10%in%c(88888,99999),
         !is.na(renda_ind_r)) %>%
  mutate(i10 =as.factor(i10),
         tipo_renda = ifelse(renda_ind_r >=3508,"renda maior que corte", "renda menor que corte"  )) %>%
  ggplot() +
  geom_bar(aes(y=i10, fill= tipo_renda), position = "fill")


amostra_expandida_1400 %>%
  filter(!i10%in%c(88888,99999),
         !is.na(renda_ind_r)) %>%
  mutate(i10 =as.factor(i10),
         tipo_renda = ifelse(renda_ind_r >=3508,"renda maior que corte", "renda menor que corte"  )) %>%
  ggplot() +
  geom_bar(aes(y=tipo_renda, fill= i10 ), position = "fill")


amostra_expandida_1400 %>%
  filter(!i10%in%c(88888,99999),
         !is.na(renda_ind_r)) %>%
  mutate(i10 =as.factor(i10),
         tipo_renda = ifelse(renda_ind_r >=3508,"renda maior que corte", "renda menor que corte"  ),
         a01ra = as.factor(a01ra)) %>%
  ggplot() +
  geom_bar(aes(y=a01ra, fill= tipo_renda ), position = "fill") +
  facet_wrap(i10~.)


amostra_expandida_1400 %>%
  filter(!i10%in%c(88888,99999),
         !is.na(renda_ind_r)) %>%
  mutate(i10 =as.factor(i10),
         tipo_renda = ifelse(renda_ind_r >=3508,"renda maior que corte", "renda menor que corte"  ),
         a01ra = as.factor(a01ra)) %>%
  ggplot() +
  geom_bar(aes(y=a01ra, fill= tipo_renda )) +
  facet_wrap(i10~.)


amostra_expandida_1400 %>%
  filter(!i10%in%c(88888,99999)) %>%
  mutate(i10 =as.factor(i10),
         a01ra = as.factor(a01ra)) %>%
  ggplot() +
  geom_bar(aes(y=a01ra, fill= i10), position = "fill") 



amostra_expandida_1400 %>%
  filter(!i10%in%c(88888,99999),
         !is.na(renda_ind_r)) %>%
  mutate(i10 =as.factor(i10),
         i09_8 = as.factor(i09_8),
         tipo_renda = ifelse(renda_ind_r >=3508,"renda maior que corte", "renda menor que corte"  )) %>%
  ggplot() +
  geom_bar(aes(y=i10, fill= tipo_renda )) +
  facet_wrap(i09_8~.)



amostra_expandida_1400 %>%
  filter(!i10%in%c(88888,99999),
         !is.na(renda_ind_r)) %>%
  mutate(i10 =as.factor(i10),
         i09_8 = as.factor(i09_8),
         mesma_regiao = as.factor(mesma_regiao)) %>%
  ggplot() +
  geom_bar(aes(y=i10, fill= mesma_regiao )) +
  facet_wrap(i09_8~.)


amostra_expandida_1400 %>%
  filter(!i10%in%c(88888,99999),
         !is.na(renda_ind_r)) %>%
  mutate(i10 =as.factor(i10),
         i09_8 = as.factor(i09_8),
         mesma_regiao = as.factor(mesma_regiao)) %>%
  ggplot() +
  geom_bar(aes(y=i10, fill= mesma_regiao ), position = "fill") +
  facet_wrap(i09_8~.)

amostra_expandida_1400 %>%
  filter(!i10%in%c(88888,99999),
         !is.na(renda_ind_r)) %>%
  mutate(i10 =as.factor(i10),
         i09_8 = as.factor(i09_8),
         mesma_regiao = as.factor(mesma_regiao)) %>%
  ggplot() +
  geom_bar(aes(y=mesma_regiao , fill= i09_8 ), position = "fill") +
  facet_wrap(i10~.)


