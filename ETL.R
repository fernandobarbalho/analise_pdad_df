library(readxl)
library(readr)
deslocamentos_escola <- read_excel("Relatorio_DF_percentual-2021.xlsx", 
                                   sheet = "A47", skip = 1, n_max=34)


PDAD_2021_Domicilios <- read_delim("PDAD_2021-Domicilios.csv", 
                                   delim = ";", escape_double = FALSE,locale = locale(decimal_mark = ",", 
                                                                                      grouping_mark = "."), trim_ws = TRUE)


PDAD_2021_Moradores <- read_delim("PDAD_2021-Moradores.csv", 
                                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                      grouping_mark = "."), trim_ws = TRUE)

PDAD_2021_Moradores <- janitor::clean_names(PDAD_2021_Moradores)

dicionario_de_variaveis_pdad_2021 <- read_excel("dicionario_de_variaveis_pdad_2021.xlsx", 
                                                sheet = "moradores")

dicionario_de_variaveis_pdad_2021 <- janitor::clean_names(dicionario_de_variaveis_pdad_2021)

nomes_regioes_administrativas <- read_excel("dicionario_de_variaveis_pdad_2021.xlsx", 
                                                sheet = "anexo_1")

nomes_regioes_administrativas <-
  nomes_regioes_administrativas %>%
  janitor::clean_names() %>%
  select(valor, descricao_do_valor) %>%
  rename(a01ra = valor,
         nome_regiao_administrativa = descricao_do_valor)

names(nomes_regioes_administrativas) <- 


dicionario_de_variaveis_pdad_2021 %>%
  filter(!is.na(coluna)) %>%
  select(coluna, descricao_da_coluna) %>%
  write_csv2("dicionario_variaveis.csv")


dicionario_variaveis<-
  dicionario_de_variaveis_pdad_2021%>%
  filter(!is.na(coluna)) %>%
  select(coluna, descricao_da_coluna) 


#Extraindo uma amostra aleatória de 8% da tabela de moradores
PDAD_2021_Moradores %>%
  slice_sample(prop = 0.08)%>%
  write_csv("amostra_moradores.csv")



