
setwd("~/GitHub/dimensionamento/11_previsões/01_scritp_tratamento")

options(scipen = 999)

knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)


if (!require(RODBC)) { install.packages(RODBC); require(RODBC) }

dremio_host <- "200.137.215.27"
dremio_port <- "31010"
dremio_uid <- "daniel"
dremio_pwd <- Sys.getenv("datalake")

channel <- odbcDriverConnect(sprintf("DRIVER=Dremio Connector;HOST=%s;PORT=%s;UID=%s;PWD=%s;AUTHENTICATIONTYPE=Basic Authentication;CONNECTIONTYPE=Direct", dremio_host, dremio_port, dremio_uid, dremio_pwd))

library(tidyverse)
library(lubridate)

consulta <- 'SELECT * FROM "@daniel"."teste meta 5"'

previsoes <- sqlQuery(channel, consulta, 
                        as.is = TRUE)

previsoes_tratado <- previsoes %>% 
                        mutate(mes_ano = ym(COMPETEN)) %>%
                        mutate(uf = if_else(substr(CODUFMUN, 1, 2) == "13", 
                                            "AM","RS")) %>% 
                        janitor::clean_names() %>% 
                        group_by(uf, municipio_pad, nomeprof, mes_ano) %>% 
                        count() %>% 
                        ungroup()

#writexl::write_xlsx(previsoes_tratado, "migracao.xlsx")
# teste -------------------------------------------------------------------


previsoes_tratado %>% 
  ggplot(aes(x = mes_ano, y = municipio_pad, col = uf, size = 3)) + geom_line() + 
  facet_wrap(~nomeprof) + theme_minimal() + xlab("Mês/ano") + ylab("Município")



