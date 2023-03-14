
library(tidyverse)
library(readxl)

equipes <- read_excel("bases/equipes.xlsx")


equipes_gyn <- equipes %>% 
                  filter(municipio == "GOIÂNIA") %>% 
                  separate(col = coordenadas_geograficas, 
                           into = c('latitude','longitude'),
                           sep = ',')

write.csv(equipes_gyn, 'equipes_gyn_tratado.csv')
