library(readxl)
library(tidyverse)
library(patchwork)


estudo_de_caso_bsb <- read_excel("~/GitHub/dimensionamento/08_APS/base_PBI/estudo de caso - bsb.xlsx")

vulnerabilidade_ra_pbf <- estudo_de_caso_bsb %>% 
                          filter(uf == 'DF') %>% 
                          group_by(RA, classificacao_pbf) %>% 
                          count() %>% 
                          group_by(RA) %>% 
                          mutate(freq = n/sum(n))

a <- vulnerabilidade_ra %>%
  filter(classificacao_pbf == 'alta vulnerabilidade') %>% 
  ggplot(aes(x = fct_reorder(RA, freq), y = freq, fill = classificacao_pbf)) + 
  geom_col(fill = 'red') + theme_minimal() + coord_flip() + 
  xlab("") + ggtitle("Alta Vulnerabilidade")

b <- vulnerabilidade_ra %>%
  filter(classificacao_pbf == 'média vulnerabilidade') %>% 
  ggplot(aes(x = fct_reorder(RA, freq), y = freq, fill = classificacao_pbf)) + 
  geom_col(fill = '#8B8000') + theme_minimal() + coord_flip() + 
  xlab("Região Administrativa") + ggtitle("Média Vulnerabilidade")

c <- vulnerabilidade_ra %>%
  filter(classificacao_pbf == 'baixa vulnerabilidade') %>% 
  ggplot(aes(x = fct_reorder(RA, freq), y = freq, fill = classificacao_pbf)) + 
  geom_col(fill = '#0A5117') + theme_minimal() + coord_flip() + 
  xlab("") + ggtitle("Média Vulnerabilidade")

a / b / c

#writexl::write_xlsx(vulnerabilidade_ra_pbf, "vulnerabilidade_pbf.xlsx")

vulnerabilidade_ra_previne <- estudo_de_caso_bsb %>% 
  filter(uf == 'DF') %>% 
  group_by(RA, classificacao_previne) %>% 
  count() %>% 
  group_by(RA) %>% 
  mutate(freq = n/sum(n))

#writexl::write_xlsx(vulnerabilidade_ra_previne, "vulnerabilidade_previne.xlsx")

# Correlacao 

# precisamos trazer os resultados sintese de oferta vs demanda e juntar a base
# que tem informacoes de vulnerabilidade 

# PBF

resultado_demanda_e_oferta <- read_excel("~/GitHub/dimensionamento/08_APS/base_PBI/resultado - demanda e oferta.xlsx")

resultados <- resultado_demanda_e_oferta %>% 
                  group_by(cnes, estabelecimento) %>% 
                  summarise(resultado = sum(resultado))


base <- estudo_de_caso_bsb %>% 
  filter(uf == 'DF') %>% 
  left_join(resultados, by = "cnes") %>% 
  mutate(resultado_inv = 80 + resultado)



base %>% 
  ggplot(aes(x = perc_benef_pbf, y = resultado, col = classificacao_pbf)) + 
  geom_smooth(method = 'lm', se = FALSE)+
  geom_point() + theme_minimal() + xlab("Percentual de Beneficiários - PBF") + 
  ylab("Resultado de Oferta - Demanda")
  

base_pbi_cor <- 
  base %>% 
  select(cnes, estabelecimento.x, RA, 
         pop_vinc, resultado, classificacao_pbf, classificacao_previne, perc_benef_pbf,
         perc_benef_previne) %>% 
  gather(key = 'indicador', value = 'perc_vuln', 
         8:9) %>% 
  mutate(indicador = if_else(indicador == 'perc_benef_pbf','PBF','Previne'))


# writexl::write_xlsx(base, 'correlacao_pbf_previne.xlsx')

cor(base$perc_benef_pbf, base$resultado)


# Previne

modelo <- lm(data = base, resultado ~ perc_benef_pbf)

summary(modelo)

base %>% 
  ggplot(aes(x = perc_benef_previne, y = resultado, col = classificacao_previne)) + 
  geom_smooth(method = 'lm', se = FALSE)+
  geom_point() + theme_minimal()


cor(base$perc_benef_previne, base$resultado)



