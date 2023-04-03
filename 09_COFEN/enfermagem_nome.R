library(genderBR)
library(RODBC)
library(tidyverse)

dremio_host <- Sys.getenv('endereco')
dremio_port <- Sys.getenv('port')
dremio_uid <- Sys.getenv('uid')
dremio_pwd <- Sys.getenv('datalake')

channel <- odbcDriverConnect(sprintf("DRIVER=Dremio Connector;HOST=%s;PORT=%s;UID=%s;PWD=%s;AUTHENTICATIONTYPE=Basic Authentication;CONNECTIONTYPE=Direct", dremio_host, dremio_port, dremio_uid, dremio_pwd))

df_enfermagem <- sqlQuery(channel, 'SELECT * FROM "Analytics Layer".Infraestrutura.Profissionais."Profissionais de enfermagem por Municipio - 122022 - Henrique"', 
                     as.is = TRUE)

df_enfermagem_s <- df_enfermagem %>% 
                    mutate(sexo = get_gender(NOMEPROF)) %>% 
                    distinct(CPF_PROF, .keep_all = TRUE) %>% 
                    filter(sexo != "NA") %>% 
                    mutate(categoria = case_when(str_detect(CBO, '^2235') ~ 'Enfermeiro',
                                                 str_detect(CBO, '^3222') ~ 'Técnico ou Auxiliar de Enfermagem'))

# agregando UF ------------------------------------------------------------

sexo_uf <- df_enfermagem_s %>% 
  group_by(uf_sigla, categoria, sexo) %>% 
  count() %>% 
  group_by(uf_sigla, categoria) %>% 
  mutate(freq = n/sum(n)) %>% 
  mutate(sexo = if_else(sexo == 'Male','Masculino','Feminino'))


writexl::write_xlsx(sexo_uf, '01_PBI/sexo.xlsx')

sexo_uf %>% 
  ggplot(aes(x = uf_sigla, y = freq, fill = sexo)) + geom_col() + 
  theme_minimal() + coord_flip() + xlab("UF") + ylab("Frequência (%)") +
  scale_y_continuous(breaks = seq(0, 1, 0.05)) +
  ggtitle("Proporção de sexo - Equipes de Enfermagem (n = 1.144.566)", 
          "Fonte: CNES-PF (01/2023) e pacote GenderBR") + 
  facet_wrap(~categoria)
  
  
  