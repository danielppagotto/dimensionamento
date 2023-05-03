
library(tidyverse)

equipes <- read_csv("C:/Users/Lapei_Cigets/Downloads/oferta_aps.csv")


oferta_equipes_ba <- equipes %>%
                    filter(municipio == '292740' |
                           municipio == '292400' |
                           municipio == '292640') %>% 
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

raiz <- oferta_equipes_ba %>% 
      ungroup() %>% 
      select(cnes_unidade, unidade, categoria, cbo_profissional, oferta_ft40) 

raiz$cnes_unidade <- as.integer(raiz$cnes_unidade)

# Comparando Oferta e Demanda -----------------------------------------------------

# 2.1 Caso BSB 

bsb_unidades <- readxl::read_excel("~/GitHub/dimensionamento/08_APS/base_PBI/equipes_gyn_tratado.xlsx") %>% 
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


#writexl::write_xlsx(resultado, 'resultado.xlsx')

# 2.2 Caso Bahia 

vulnerabilidade <- read_excel("base_PBI/vulnerabilidade.xlsx")

oferta_demanda_ba <- vulnerabilidade %>% 
                    filter(ibge == '292740' |
                           ibge == '292400' |
                           ibge == '292640') %>% 
                select(municipio, uf, ibge, cnes, estabelecimento, pop_vinc, 
                       qt_bnf_auxilio_brasil, classificacao_pbf, medicos_pbf, 
                       enfermeiros_pbf, tecnicos_pbf, acs_pbf) %>% 
  gather(key = 'profissional',
         value = 'qtd_prof_necessario',
         c(9:12)) %>% 
  mutate(categoria = case_when(str_detect(profissional, '^med') ~ 'Médico',
                               str_detect(profissional, '^enfe') ~ 'Enfermeiro',
                               str_detect(profissional, '^tecn') ~ 'Tecnicos',
                               str_detect(profissional, '^acs') ~ 'ACS')) %>% 
  left_join(raiz, by = c("cnes"="cnes_unidade",
                         "categoria"="categoria")) 

oferta_demanda_ba$oferta_ft40[is.na(oferta_demanda_ba$oferta_ft40)] <- 0
oferta_demanda_ba$qtd_prof_necessario[is.na(oferta_demanda_ba$qtd_prof_necessario)] <- 0

resultado_ba <- oferta_demanda_ba %>% 
  mutate(resultado = round((oferta_ft40 - qtd_prof_necessario), 2),
         oferta_ft40 = round(oferta_ft40,2),
         qtd_prof_necessario = round(qtd_prof_necessario,2)) %>% 
  select(-unidade, -cbo_profissional)

writexl::write_xlsx(resultado_ba, "oferta_demanda_ba.xlsx")  
