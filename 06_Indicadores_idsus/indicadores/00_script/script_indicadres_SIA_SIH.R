library(tidyverse)
library(RODBC)

dremio_host <- "200.137.215.27"
dremio_port <- "31010"
dremio_uid <- "daniel"
dremio_pwd <- Sys.getenv("datalake")

channel <- odbcDriverConnect(sprintf("DRIVER=Dremio Connector;HOST=%s;PORT=%s;UID=%s;PWD=%s;AUTHENTICATIONTYPE=Basic Authentication;CONNECTIONTYPE=Direct", dremio_host, dremio_port, dremio_uid, dremio_pwd))

# Indicador 1 - procedimentos de média complexidade por população ----
# Comentário
# Baixando dados - SIA-PA ----- 

consulta_sia <- 
  "SELECT PA_MUNPCN, COUNT(*) AS TOTAL 
   FROM Dados.sia.PA
   WHERE substr(PA_CMP, 1, 4) = '2020' AND 
      (PA_PROC_ID = '0201010151' OR 
       PA_PROC_ID = '0201010160' OR 
       PA_PROC_ID = '201010585' OR
       PA_PROC_ID = '201010607' OR
       PA_PROC_ID = '201010666' OR
       PA_PROC_ID = '202030059' OR
       PA_PROC_ID = '202030237' OR
       PA_PROC_ID = '202031080' OR
       PA_PROC_ID = '203010043' OR
       PA_PROC_ID = '203020014' OR
       PA_PROC_ID = '205010032' OR
       PA_PROC_ID = '405030045' OR
       PA_PROC_ID = '405050097' OR
       PA_PROC_ID = '405050100' OR
       PA_PROC_ID = '405050119' OR
       PA_PROC_ID = '405050151' OR
       PA_PROC_ID = '405050372' OR
       PA_PROC_ID = '409040240' OR
       PA_PROC_ID = '409050083' OR
       PA_PROC_ID = '506010023' OR
       PA_PROC_ID = '506010031' OR
       PA_PROC_ID = '506010040') 
       AND (PA_DOCORIG = 'I' OR PA_DOCORIG = 'P' OR
       PA_DOCORIG = 'S')
   GROUP BY PA_MUNPCN"
  
sia_pa <- sqlQuery(channel, consulta_sia)

# Baixando dados - SIH

consulta_sih <- 
  "SELECT MUNIC_RES, DT_INTER, COUNT(*) AS TOTAL 
    FROM Dados.sih.RD
    WHERE (ANO_CMPT = '2020' OR ANO_CMPT = '2021') 
    AND (PROC_REA = '0201010151' OR 
         PROC_REA = '0201010160'
         ) AND (COBRANCA = '11' OR
                COBRANCA = '12') 
    GROUP BY MUNIC_RES, DT_INTER"

sih <- sqlQuery(channel, consulta_sih)


# Indicador 2 - Razão de internações clínico-cirúrgicas de média complexidade e população residente ----

# Baixando dados - SIH - indicador 2

consulta2_sih <- 
  "SELECT MUNIC_RES, DT_INTER, COUNT(*) AS TOTAL 
    FROM (SELECT MUNIC_RES, ANO_CMPT, CAST(PROC_REA AS INTEGER) AS PROC_REA_C, 
                 COBRANCA
                 FROM Dados.sih.RD)
    WHERE (ANO_CMPT = '2020' OR ANO_CMPT = '2021') 
    AND (
            (PROC_REA_C >= 201010010 AND PROC_REA_C <= 201010159) OR
            (PROC_REA_C >= 201010170 AND PROC_REA_C <= 201010579) OR
            (PROC_REA_C >= 201010590 AND PROC_REA_C <= 201010599) OR
            (PROC_REA_C >= 201010610 AND PROC_REA_C <= 201010659) OR
            (PROC_REA_C >= 201010670 AND PROC_REA_C <= 202030229) OR
            (PROC_REA_C >= 202030240 AND PROC_REA_C <= 205010029) OR
            (PROC_REA_C >= 205010040 AND PROC_REA_C <= 211099999) OR
            (PROC_REA_C >= 211120010 AND PROC_REA_C <= 211129999) OR
            (PROC_REA_C >= 303010010 AND PROC_REA_C <= 303099999) OR
            (PROC_REA_C >= 303110010 AND PROC_REA_C <= 303169999) OR
            (PROC_REA_C >= 303180010 AND PROC_REA_C <= 305029999) OR
            (PROC_REA_C >= 306020010 AND PROC_REA_C <= 306029999) OR
            (PROC_REA_C >= 308010010 AND PROC_REA_C <= 309069999) OR
            (PROC_REA_C >= 303100010 AND PROC_REA_C <= 303109999) OR
            (PROC_REA_C >= 411010010 AND PROC_REA_C <= 411010019) OR
            (PROC_REA_C >= 411010050 AND PROC_REA_C <= 411020059) OR
            (PROC_REA_C >= 401010010 AND PROC_REA_C <= 405050089) OR
            (PROC_REA_C >= 405050120 AND PROC_REA_C <= 405050149) OR
            (PROC_REA_C >= 405050160 AND PROC_REA_C <= 405050369) OR
            (PROC_REA_C >= 405050380 AND PROC_REA_C <= 409040239) OR
            (PROC_REA_C >= 409040250 AND PROC_REA_C <= 409050079) OR
            (PROC_REA_C >= 409050090 AND PROC_REA_C <= 410019999) OR
            (PROC_REA_C >= 412010010 AND PROC_REA_C <= 414019999) OR
            (PROC_REA_C >= 415010010 AND PROC_REA_C <= 416139999)
        )
    AND (COBRANCA = '11' OR
                COBRANCA = '12') 
    GROUP BY MUNIC_RES, DT_INTER"


