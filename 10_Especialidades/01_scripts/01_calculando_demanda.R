library(tidyverse)
library(readxl)
library(RODBC)

# Calculando demanda 

# puxando dados das regiões de saude primeiro 

dremio_host <- Sys.getenv('endereco')
dremio_port <- Sys.getenv('port')
dremio_uid <- Sys.getenv('uid')
dremio_pwd <- Sys.getenv('datalake')

channel <- odbcDriverConnect(sprintf("DRIVER=Dremio Connector;HOST=%s;PORT=%s;UID=%s;PWD=%s;AUTHENTICATIONTYPE=Basic Authentication;CONNECTIONTYPE=Direct", dremio_host, dremio_port, dremio_uid, dremio_pwd))

df_pop_regiao <- sqlQuery(channel, 'SELECT * FROM "Analytics Layer".Territorial."População por região de saúde"', 
                          as.is = TRUE)

df_pop_regiao$populacao <- as.numeric(df_pop_regiao$populacao)

# calculando com base em parametros 

# parametros padrões

demanda <- df_pop_regiao %>% 
                mutate(m_225130 = populacao/2000,
                       m_225142 = populacao/2000) %>% 
                gather(key = 'CBO', value = 'demanda',
                       6:7) %>% 
                mutate(CBO = str_sub(CBO, 3)) %>% 
                mutate(parametro = "Parâmetro padrão")

# parametros 5, 10 de aumento ou diminuicao da populacao

demanda_5 <- df_pop_regiao %>% 
  mutate(populacao = populacao * 1.05) %>% 
  mutate(m_225130 = populacao/(2000),
         m_225142 = populacao/(2000)) %>% 
  gather(key = 'CBO', value = 'demanda',
         6:7) %>% 
  mutate(CBO = str_sub(CBO, 3)) %>% 
  mutate(parametro = "Aumento da populacao em 5%")





# puxando os dados de macrorregiões de saude 

parametros_demanda <- rbind(demanda, demanda_5)

writexl::write_xlsx(parametros_demanda, "09_basesPBI/parametros_demanda.xlsx")
