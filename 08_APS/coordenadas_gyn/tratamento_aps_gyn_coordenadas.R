
library(tidyverse)
library(readxl)

equipes <- read_excel("coordenadas_gyn/equipes_bsb_gyn.xlsx")


# GYN ---------------------------------------------------------------------


equipes_gyn <- equipes %>% 
                  filter(municipio == "GOIÂNIA") %>% 
                  separate(col = coordenadas_geograficas, 
                           into = c('latitude','longitude'),
                           sep = ',')

# write.csv(equipes_gyn, 'equipes_gyn_tratado.csv')
# writexl::write_xlsx(equipes_gyn, 'equipes_gyn_tratado.xlsx')


# BSB ---------------------------------------------------------------------


equipes_bsb <- equipes %>% 
                  filter(municipio == "BRASÍLIA") %>% 
                  separate(col = coordenadas_geograficas, 
                           into = c('latitude','longitude'),
                           sep = ',')
