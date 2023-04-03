library(tidyverse)
library(readxl)
library(RODBC)
library(rio)


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
                       m_225142 = populacao/2000,
                       m_225124 = populacao/4000,
                       m_225125 = populacao/4000,
                       m_225250 = populacao/4000,
                       m_225170 = populacao/4184,
                       m_225225 = populacao/6250,
                       m_225151 = populacao/10000,
                       m_225270 = populacao/10000,
                       m_225120 = populacao/15385,
                       m_225133 = populacao/17241,
                       m_225265 = populacao/25000,
                       m_225112 = populacao/28571,
                       m_225121 = populacao/33333,
                       m_225127 = populacao/33333,
                       m_225275 = populacao/33333,
                       m_225285 = populacao/33333,
                       m_225109 = populacao/38461,
                       m_225165 = populacao/40000,
                       m_225135 = populacao/50000,
                       m_225220 = populacao/50000,
                       m_225230 = populacao/50000,
                       m_225235 = populacao/50000,
                       m_225280 = populacao/50000,
                       m_225115 = populacao/66666,
                       m_225155 = populacao/66666,
                       m_225103 = populacao/100000,
                       m_225105 = populacao/100000,
                       m_225110 = populacao/100000,
                       m_225136 = populacao/100000,
                       m_225140 = populacao/100000,
                       m_225180 = populacao/100000,
                       m_225185 = populacao/100000,
                       m_225195 = populacao/100000,
                       m_225210 = populacao/100000,
                       m_225215 = populacao/100000,
                       m_225240 = populacao/100000,
                       m_225255 = populacao/100000,
                       m_225315 = populacao/200000,
                       m_225340 = populacao/200000,
                       m_225175 = populacao/400000,
                       m_225150 = populacao/33333,
                       m_225203 = populacao/100000,
                       m_225320 = populacao/200000,
                       m_225    = populacao/2000
                       ) %>% 
                gather(key = 'CBO', value = 'demanda',
                       6:50) %>% 
                mutate(CBO = str_sub(CBO, 3)) %>% 
                mutate(parametro = "Parâmetro padrão")

# parametros 5, 10 de aumento ou diminuicao da populacao

# aumento 5%

demanda_aum_5 <- df_pop_regiao %>% 
  mutate(populacao = populacao * 1.05) %>% 
  mutate(m_225130 = populacao/(2000),
         m_225142 = populacao/(2000),
         m_225124 = populacao/(4000),
         m_225125 = populacao/(4000),
         m_225250 = populacao/(4000),
         m_225170 = populacao/(4184),
         m_225225 = populacao/(6250),
         m_225151 = populacao/(10000),
         m_225270 = populacao/(10000),
         m_225120 = populacao/(15385),
         m_225133 = populacao/(17241),
         m_225265 = populacao/(25000),
         m_225112 = populacao/(28571),
         m_225121 = populacao/(33333),
         m_225127 = populacao/(33333),
         m_225275 = populacao/(33333),
         m_225285 = populacao/(33333),
         m_225109 = populacao/(38461),
         m_225165 = populacao/(40000),
         m_225135 = populacao/(50000),
         m_225220 = populacao/(50000),
         m_225230 = populacao/(50000),
         m_225235 = populacao/(50000),
         m_225280 = populacao/(50000),
         m_225115 = populacao/(66666),
         m_225155 = populacao/(66666),
         m_225103 = populacao/(100000),
         m_225105 = populacao/(100000),
         m_225110 = populacao/(100000),
         m_225136 = populacao/(100000),
         m_225140 = populacao/(100000),
         m_225180 = populacao/(100000),
         m_225185 = populacao/(100000),
         m_225195 = populacao/(100000),
         m_225210 = populacao/(100000),
         m_225215 = populacao/(100000),
         m_225240 = populacao/(100000),
         m_225255 = populacao/(100000),
         m_225315 = populacao/(200000),
         m_225340 = populacao/(200000),
         m_225175 = populacao/(400000),
         m_225150 = populacao/(33333),
         m_225203 = populacao/(100000),
         m_225320 = populacao/(200000),
         m_225    = populacao/(2000)) %>% 
  gather(key = 'CBO', value = 'demanda',
         6:50) %>% 
  mutate(CBO = str_sub(CBO, 3)) %>% 
  mutate(parametro = "Aumento da populacao em 5%")

#aumento 10%

demanda_aum_10 <- df_pop_regiao %>% 
  mutate(populacao = populacao * 1.10) %>% 
  mutate(m_225130 = populacao/(2000),
         m_225142 = populacao/(2000),
         m_225124 = populacao/(4000),
         m_225125 = populacao/(4000),
         m_225250 = populacao/(4000),
         m_225170 = populacao/(4184),
         m_225225 = populacao/(6250),
         m_225151 = populacao/(10000),
         m_225270 = populacao/(10000),
         m_225120 = populacao/(15385),
         m_225133 = populacao/(17241),
         m_225265 = populacao/(25000),
         m_225112 = populacao/(28571),
         m_225121 = populacao/(33333),
         m_225127 = populacao/(33333),
         m_225275 = populacao/(33333),
         m_225285 = populacao/(33333),
         m_225109 = populacao/(38461),
         m_225165 = populacao/(40000),
         m_225135 = populacao/(50000),
         m_225220 = populacao/(50000),
         m_225230 = populacao/(50000),
         m_225235 = populacao/(50000),
         m_225280 = populacao/(50000),
         m_225115 = populacao/(66666),
         m_225155 = populacao/(66666),
         m_225103 = populacao/(100000),
         m_225105 = populacao/(100000),
         m_225110 = populacao/(100000),
         m_225136 = populacao/(100000),
         m_225140 = populacao/(100000),
         m_225180 = populacao/(100000),
         m_225185 = populacao/(100000),
         m_225195 = populacao/(100000),
         m_225210 = populacao/(100000),
         m_225215 = populacao/(100000),
         m_225240 = populacao/(100000),
         m_225255 = populacao/(100000),
         m_225315 = populacao/(200000),
         m_225340 = populacao/(200000),
         m_225175 = populacao/(400000),
         m_225150 = populacao/(33333),
         m_225203 = populacao/(100000),
         m_225320 = populacao/(200000),
         m_225    = populacao/(2000)) %>% 
  gather(key = 'CBO', value = 'demanda',
         6:50) %>% 
  mutate(CBO = str_sub(CBO, 3)) %>% 
  mutate(parametro = "Aumento da populacao em 10%")

#diminuicao 5%

demanda_dim_5 <- df_pop_regiao %>% 
  mutate(populacao = populacao * 0.95) %>% 
  mutate(m_225130 = populacao/(2000),
         m_225142 = populacao/(2000),
         m_225124 = populacao/(4000),
         m_225125 = populacao/(4000),
         m_225250 = populacao/(4000),
         m_225170 = populacao/(4184),
         m_225225 = populacao/(6250),
         m_225151 = populacao/(10000),
         m_225270 = populacao/(10000),
         m_225120 = populacao/(15385),
         m_225133 = populacao/(17241),
         m_225265 = populacao/(25000),
         m_225112 = populacao/(28571),
         m_225121 = populacao/(33333),
         m_225127 = populacao/(33333),
         m_225275 = populacao/(33333),
         m_225285 = populacao/(33333),
         m_225109 = populacao/(38461),
         m_225165 = populacao/(40000),
         m_225135 = populacao/(50000),
         m_225220 = populacao/(50000),
         m_225230 = populacao/(50000),
         m_225235 = populacao/(50000),
         m_225280 = populacao/(50000),
         m_225115 = populacao/(66666),
         m_225155 = populacao/(66666),
         m_225103 = populacao/(100000),
         m_225105 = populacao/(100000),
         m_225110 = populacao/(100000),
         m_225136 = populacao/(100000),
         m_225140 = populacao/(100000),
         m_225180 = populacao/(100000),
         m_225185 = populacao/(100000),
         m_225195 = populacao/(100000),
         m_225210 = populacao/(100000),
         m_225215 = populacao/(100000),
         m_225240 = populacao/(100000),
         m_225255 = populacao/(100000),
         m_225315 = populacao/(200000),
         m_225340 = populacao/(200000),
         m_225175 = populacao/(400000),
         m_225150 = populacao/(33333),
         m_225203 = populacao/(100000),
         m_225320 = populacao/(200000),
         m_225    = populacao/(2000)) %>% 
  gather(key = 'CBO', value = 'demanda',
         6:50) %>% 
  mutate(CBO = str_sub(CBO, 3)) %>% 
  mutate(parametro = "Diminuição da populacao em 5%")

#diminuicao 10%

demanda_dim_10 <- df_pop_regiao %>% 
  mutate(populacao = populacao * 0.90) %>% 
  mutate(m_225130 = populacao/(2000),
         m_225142 = populacao/(2000),
         m_225124 = populacao/(4000),
         m_225125 = populacao/(4000),
         m_225250 = populacao/(4000),
         m_225170 = populacao/(4184),
         m_225225 = populacao/(6250),
         m_225151 = populacao/(10000),
         m_225270 = populacao/(10000),
         m_225120 = populacao/(15385),
         m_225133 = populacao/(17241),
         m_225265 = populacao/(25000),
         m_225112 = populacao/(28571),
         m_225121 = populacao/(33333),
         m_225127 = populacao/(33333),
         m_225275 = populacao/(33333),
         m_225285 = populacao/(33333),
         m_225109 = populacao/(38461),
         m_225165 = populacao/(40000),
         m_225135 = populacao/(50000),
         m_225220 = populacao/(50000),
         m_225230 = populacao/(50000),
         m_225235 = populacao/(50000),
         m_225280 = populacao/(50000),
         m_225115 = populacao/(66666),
         m_225155 = populacao/(66666),
         m_225103 = populacao/(100000),
         m_225105 = populacao/(100000),
         m_225110 = populacao/(100000),
         m_225136 = populacao/(100000),
         m_225140 = populacao/(100000),
         m_225180 = populacao/(100000),
         m_225185 = populacao/(100000),
         m_225195 = populacao/(100000),
         m_225210 = populacao/(100000),
         m_225215 = populacao/(100000),
         m_225240 = populacao/(100000),
         m_225255 = populacao/(100000),
         m_225315 = populacao/(200000),
         m_225340 = populacao/(200000),
         m_225175 = populacao/(400000),
         m_225150 = populacao/(33333),
         m_225203 = populacao/(100000),
         m_225320 = populacao/(200000),
         m_225    = populacao/(2000)) %>% 
  gather(key = 'CBO', value = 'demanda',
         6:50) %>% 
  mutate(CBO = str_sub(CBO, 3)) %>% 
  mutate(parametro = "Diminuição da populacao em 10%")


# puxando os dados de macrorregiões de saude 

parametros_demanda <- rbind(demanda, demanda_aum_5, demanda_aum_10, demanda_dim_5, demanda_dim_10)

write.csv(parametros_demanda,"09_basesPBI/parametros_demanda.csv")

