library(tidyverse)
library(RODBC)

dremio_host <- "200.137.215.27"
dremio_port <- "31010"
dremio_uid <- "daniel"
dremio_pwd <- Sys.getenv("datalake")

channel <- odbcDriverConnect(sprintf("DRIVER=Dremio Connector;HOST=%s;PORT=%s;UID=%s;PWD=%s;AUTHENTICATIONTYPE=Basic Authentication;CONNECTIONTYPE=Direct", dremio_host, dremio_port, dremio_uid, dremio_pwd))

# Indicador 1 - procedimentos de média complexidade por população ----

# Baixando dados - SIA-PA ----- 

consulta_sia <- 
  "SELECT PA_MUNPCN, COUNT(*) AS TOTAL 
   FROM Dados.sia.PA
   WHERE substr(PA_CMP, 1, 4) = '2020' AND 
      (PA_PROC_ID = '0201010151' OR 
       PA_PROC_ID = '0201010160' OR 
       PA_PROC_ID = '') AND 
      (PA_DOCORIG = 'I' OR PA_DOCORIG = 'P' OR
       PA_DOCORIG = 'S')
   GROUP BY PA_MUNPCN"
  
sia_pa <- sqlQuery(channel, consulta_sia)

# Baixando dados - SIH

consulta_sih <- 
  "SELECT MUNIC_RES, COUNT(*) AS TOTAL 
    FROM Dados.sih.RD
    WHERE (ANO_CMPT = '2020' OR ANO_CMPT = '2021') 
    AND (PROC_REA = '0201010151' OR 
         PROC_REA = '0201010160' OR
         ) AND (COBRANCA = '11' OR
                COBRANCA = '12') 
    GROUP BY MUNIC_RES"

# AND substr(PROC_REA, 1, 7)

sih <- sqlQuery(channel, consulta_sih)
