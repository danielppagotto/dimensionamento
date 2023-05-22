library(tidyverse)

options(scipen = 999)

setwd("~/GitHub/dimensionamento/11_previsões")

previsoes <- read_csv("GitHub/dimensionamento/11_previsões/resultados_regiao_saude_BR.csv")


previsoes_tratado <- 
  previsoes %>% 
  select(-ep_stop, -seed, -ep,
         -batch_size, -neuron_config,
         -n_clusters, -cluster, -`Unnamed: 0`) %>% 
  gather(key = "mes_ano", value = "qtd",
         5:124) %>% 
  mutate(data = my(mes_ano))

# writexl::write_xlsx(previsoes_tratado, "previsoes.xlsx")


# gerando essa planilha apenas para o estudo da ABEFACO

previsoes_go <- previsoes_tratado %>% 
                    filter(Estado == "GO")

# writexl::write_xlsx(previsoes_go, "previsoes_go.xlsx")
