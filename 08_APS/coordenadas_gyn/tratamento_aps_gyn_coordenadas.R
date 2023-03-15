
library(tidyverse)
library(readxl)

unidades_georreferenciadas <- read_excel("~/GitHub/dimensionamento/08_APS/bases/unidades_georreferenciadas.xlsx", 
                                         col_types = c("text", "text", "text", 
                                                       "numeric", "text", "text", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "text", "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric"))

# BSB e GYN ---------------------------------------------------------------------


equipes_gyn <- unidades_georreferenciadas %>% 
                  filter(municipio == "GOIÂNIA" | municipio == "BRASÍLIA") %>% 
                  separate(col = coordenadas_geograficas, 
                           into = c('latitude','longitude'),
                           sep = ',') %>% 
                  filter(latitude != "NA")

setwd("~/GitHub/dimensionamento/08_APS/base_PBI")


# write.csv(equipes_gyn, 'equipes_gyn_tratado.csv')
 writexl::write_xlsx(equipes_gyn, 'equipes_gyn_tratado.xlsx')


