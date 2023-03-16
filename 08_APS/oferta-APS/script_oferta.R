
library(tidyverse)

equipes <- read_csv("oferta-APS/oferta_aps.csv")


oferta_equipes_bsb <- equipes %>%
                    filter(str_detect(municipio, '^53')) %>% 
                    filter(grupo_equipe == 'ESFN' | grupo_equipe == 'EAPN') %>% 
                    mutate(CH = QT_CARGA_HORARIA_AMBULATORIAL + QT_CARGA_HORARIA_AMBULATORIAL +
                             QT_CARGA_HORARIA_OUTROS) %>% 
                    group_by(cnes_unidade, unidade, tipo_equipe,
                             cbo_profissional, ocupacao_profissional) %>% 
                    summarise(oferta_ch = sum(CH),
                              oferta_ft40 = oferta_ch/40) %>% 
                    filter(str_detect(cbo_profissional, '^225') | 
                           str_detect(cbo_profissional, '^2235')|
                           str_detect(cbo_profissional, '^3222')|
                             cbo_profissional == '515105') %>% 
                    mutate(categoria = case_when(str_detect(cbo_profissional, '^225') ~ 'Médico',
                                                 str_detect(cbo_profissional, '^2235') ~ 'Enfermeiro',
                                                 str_detect(cbo_profissional, '^3222') ~ 'Tecnicos',
                                                 str_detect(cbo_profissional, '^5151') ~ 'ACS')) 

raiz <- oferta_equipes_bsb %>% 
      ungroup() %>% 
      select(cnes_unidade, unidade, categoria, cbo_profissional, oferta_ft40) 

raiz$cnes_unidade <- as.integer(raiz$cnes_unidade)

# Comparando Brasília -----------------------------------------------------

bsb_unidades <- readxl::read_excel("base_PBI/equipes_gyn_tratado.xlsx") %>% 
                    filter(uf == 'DF')
  
prof_necessarios_bsb <- bsb_unidades %>% 
                          select(cnes, estabelecimento, latitude, longitude,
                                 medicos, enfermeiros, tecnicos, acs) %>% 
                          gather(key = 'profissional',
                                 value = 'qtd_prof_necessario',
                                 c(5:8)) %>% 
                          mutate(categoria = case_when(str_detect(profissional, '^med') ~ 'Médico',
                                                      str_detect(profissional, '^enfe') ~ 'Enfermeiro',
                                                      str_detect(profissional, '^tecn') ~ 'Tecnicos',
                                                      str_detect(profissional, '^acs') ~ 'ACS')) %>% 
                          left_join(raiz, by = c("cnes"="cnes_unidade",
                                                 "categoria"="categoria")) 

prof_necessarios_bsb$oferta_ft40[is.na(prof_necessarios_bsb$oferta_ft40)] <- 0
prof_necessarios_bsb$qtd_prof_necessario[is.na(prof_necessarios_bsb$qtd_prof_necessario)] <- 0

resultado <- prof_necessarios_bsb %>% 
  mutate(resultado = oferta_ft40 - qtd_prof_necessario)

resultado %>% 
  group_by(profissional) %>% 
  summarise(total = sum(resultado))


writexl::write_xlsx(resultado, 'resultado.xlsx')
