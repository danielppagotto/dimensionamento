library(tidyverse)

setwd("~/GitHub/dimensionamento/11_previsões/01_scritp_tratamento")

options(scipen = 999)

previsoes <- read_csv("resultados_regiao_saude_BR.csv")


previsoes_tratado <- 
  previsoes %>% 
  select(-ep_stop, -seed, -ep,
         -batch_size, -neuron_config,
         -n_clusters, -cluster, -`Unnamed: 0`) %>% 
  gather(key = "mes_ano", value = "qtd",
         5:124) %>% 
  mutate(data = my(mes_ano))


# pegando dados de Goiás para o artigo da HRH

previsoes_go <- 
  previsoes_tratado %>% 
  filter(Estado == "GO")

writexl::write_xlsx(previsoes_go, "previsoes_go.xlsx")

