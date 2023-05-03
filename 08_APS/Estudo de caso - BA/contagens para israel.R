library(readxl)
library(tidyverse)

# analises israel


oferta_demanda_ba <- read_excel("Estudo de caso - BA/oferta_demanda_ba.xlsx")

profissionais_resultados <- oferta_demanda_ba %>% 
                                group_by(uf, municipio, estabelecimento,
                                         categoria) %>% 
                                summarise(necessario = sum(qtd_prof_necessario),
                                          oferta = sum(oferta_ft40))
  
writexl::write_xlsx(profissionais_resultados, "profissionais_ba.xlsx")
