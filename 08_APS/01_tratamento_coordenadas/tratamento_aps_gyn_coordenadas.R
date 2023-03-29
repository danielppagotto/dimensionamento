
library(tidyverse)
library(readxl)

unidades_georreferenciadas <- read_excel("~/GitHub/dimensionamento/08_APS/bases/unidades_georreferenciadas.xlsx", 
                                         col_types = c("text", "text", "text", 
                                                       "numeric", "text", "text", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "text", "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric"))

# BSB e GYN ----------------------------------------------------------------


equipes_gyn_bsb <- unidades_georreferenciadas %>% 
                  filter(municipio == "GOIÂNIA" | municipio == "BRASÍLIA") %>% 
                  separate(col = coordenadas_geograficas, 
                           into = c('latitude','longitude'),
                           sep = ',') %>% 
                  filter(latitude != "NA") %>% 
                  select(cnes, latitude, longitude)

# Juntando com tabela de vulnerabilidade ----------------------------------

vulnerabilidade <- read_excel("~/GitHub/dimensionamento/08_APS/00_vulnerabilidade/equipes.xlsx")

equpes_gyn_tratado <- 
  equipes_gyn_bsb %>% 
  left_join(vulnerabilidade, by = c('cnes'))

# write.csv(equipes_gyn, 'equipes_gyn_tratado.csv')
# writexl::write_xlsx(equpes_gyn_tratado, '~/GitHub/dimensionamento/08_APS/base_PBI/equipes_gyn_tratado.xlsx')
 