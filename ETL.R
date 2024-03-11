library(readxl)
deslocamentos_escola <- read_excel("Relatorio_DF_percentual-2021.xlsx", 
                                   sheet = "A47", skip = 1, n_max=34)


library(readr)
PDAD_2021_Domicilios <- read_delim("PDAD_2021-Domicilios.csv", 
                                   delim = ";", escape_double = FALSE,locale = locale(decimal_mark = ",", 
                                                                                      grouping_mark = "."), trim_ws = TRUE)


library(readr)
PDAD_2021_Moradores <- read_delim("PDAD_2021-Moradores.csv", 
                                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                      grouping_mark = "."), trim_ws = TRUE)


