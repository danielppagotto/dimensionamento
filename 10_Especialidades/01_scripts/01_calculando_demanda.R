library(tidyverse)
library(readxl)

# Calculando demanda 
# Base de parametros 

CBO_MEDICOS <- read_excel("00_bases/CBO_MEDICOS.xlsx") %>% 
                  gather(key = 'opcao_parametro',
                         value = 'parametro',
                         3:9) 

# puxando dados das regiões de saude primeiro 





# puxando os dados de macrorregiões de saude 


writexl::write_xlsx(CBO_MEDICOS, "09_basesPBI/parametros_demanda.xlsx")
