
library(tidyverse)

equipes_1_ <- read_excel("C:/Users/Lapei_Cigets/Downloads/equipes (1).xlsx", 
                             col_types = c("text", "text", "text", 
                                             "numeric", "text", "text", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "text", "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))

equipes_gyn <- equipes_1_ %>% 
                  filter(municipio == "GOIÂNIA") %>% 
                  separate(col = coordenadas_geograficas, 
                           into = c('latitude','longitude'),
                           sep = ',')

write.csv(equipes_gyn, 'equipes_gyn_tratado.csv')
