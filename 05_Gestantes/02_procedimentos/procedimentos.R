# Carregando pacotes -----
library(tidyverse)
library(lubridate) 
library(modeltime)
library(tidymodels)
library(timetk)
library(readxl)

# Inserindo local das bases ----

previsoes_go <- read_excel("01_previsoes_nascidos/previsoes_go.xlsx")

# Tempos ------------------------------------------------------------------


nascimentos_previsao_tempos <- read_excel("~/GitHub/prophet_gestantes/bases/nascimentos_previsao_tempos.xlsx")

nascimentos_previsao_atual <- nascimentos_previsao_tempos %>% 
                                  select(regiao, mes_ano_data, total) %>% 
                                  mutate(status = "previsto") %>% 
                                  rename(macrorregiao = regiao, 
                                         data = mes_ano_data)


nascimentos_previsao_atual <- rbind(nascimentos_previsao_atual, nascimentos_go)

nascimentos_previsao_atual <- nascimentos_previsao_atual %>% 
                                  mutate(macrorregiao = case_when(macrorregiao == "Centro Norte" | 
                                                                  macrorregiao == "Macrorregi?o Centro-Norte" ~ "Centro-Norte",
                                                                  macrorregiao == "Centro Oeste" | 
                                                                  macrorregiao == "macrorregi?o Centro-Oeste" ~ "Centro-Oeste",
                                                                  macrorregiao == "Macrorregi?o Centro Sudeste" | 
                                                                  macrorregiao == "Centro Sudeste" ~ "Centro-Sudeste",
                                                                  macrorregiao == "Macrorregi?o Nordeste" | macrorregiao == "Nordeste" ~ "Nordeste",
                                                                  macrorregiao == "Macrorregi?o Sudoeste" | macrorregiao == "Sudoeste" ~ "Sudoeste"))

nascimentos_previsao_atual %>% 
  group_by(macrorregiao) %>% 
  count()

# total de nascimentos  ---------------------------------------------------


nascimentos_previsao_atual %>% 
  ggplot(aes(x = data, y = total, col = macrorregiao)) + geom_line(size = 1) + 
  theme_minimal() + geom_vline(xintercept=as.numeric(ymd("2021-01-01")), linetype="dashed",
                               color = "blue", size=1.5) +
  theme(text = element_text(size = 22)) + xlab("Ano") +
  theme(legend.position = 'bottom')




# tempo de procedimentos  -------------------------------------------------

tempos_procedimentos <- read_excel("~/GitHub/prophet_gestantes/bases/nascimentos_previsao_tempos.xlsx", 
                                          sheet = "tempo de procedimentos")


tempos <- tempos_procedimentos %>% 
  select(-mes_ano, -ano) %>% 
  group_by(regiao) %>% 
  summarise_all(mean) %>% 
  gather(key = "Procedimento",
         value = "Tempo em horas",
         2:14) %>% 
  separate(Procedimento, c("Procedimento","Nível de atenção"), ",") %>% 
  mutate(atencao = as.numeric(`Nível de atenção`)) %>% 
  mutate(Atencao = case_when(atencao == 1 ~ "Primária",
                             atencao == 2 ~ "Especializada"))


tempos %>% 
  ggplot(aes(x = fct_reorder(Procedimento,`Tempo em horas`), 
             y = `Tempo em horas`, fill = Atencao)) + 
  geom_col() + coord_flip() + theme_minimal() + 
  xlab("Procedimento") + ggtitle("Tempo m?dio por procedimento", 
                                 "M?dia de tempos dos tr?s anos projetados") + 
  facet_grid(~regiao) + theme(text = element_text(size = 25)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# tempos de profissionais -------------------------------------------------


tempos_procedimentos <- nascimentos_previsao_tempos %>% 
  select(ano, regiao, starts_with("tempo_"), pessoal40h, medico_ab,
         medico_esp,	enf_ab) %>% 
  group_by(ano, regiao) %>% 
  summarise_all(sum) 


# tempo_profissionais <- tempos_procedimentos %>% 
#                           mutate(tempo_medico_ab = (0.5 * tempo_consulta_prenatal +
#                                                     0.5 * tempo_consulta_puerperal +
#                                                     0.5 * tempo_acoes_educativas + 
#                                                     tempo_ultrassonografia_obstetrica +
#                                                     tempo_exame_citopatologico)/960, 
#                                 tempo_medico_esp = (tempo_consulta_especializada +
#                                                     tempo_ecg + 
#                                                     tempo_ultrassom_obstetrico_dopler + 
#                                                     tempo_ultrassom_obstetrico + 
#                                                     tempo_tococardiografia + 
#                                                     tempo_parto_cesarea + 
#                                                     tempo_parto_normal_sem_instrumenta??o)/960,
#                                 tempo_enfermeiro = (0.5 * tempo_consulta_prenatal +
#                                                     0.5 * tempo_consulta_puerperal +
#                                                     0.5 * tempo_acoes_educativas)/960) %>% 
#                           select(batch, ano, regiao, tempo_medico_ab, tempo_medico_esp, 
#                                  tempo_enfermeiro) %>% 
#                           gather(key = "Profissional",
#                                  value = "total",
#                                  4:6) %>% ungroup() %>% select(-ano)




demanda_profissionais <- tempos_procedimentos %>% 
    select(ano, regiao, medico_ab, medico_esp, enf_ab) %>% 
  gather(key = "Profissional",
         value = "total",
         3:5) %>% ungroup() %>% 
  mutate(cenario = "demanda")



# 
# tempo_profissionais$batch <- as.factor(tempo_profissionais$batch) 
# tempo_profissionais$batch <- ordered(tempo_profissionais$batch, 
#                                     levels = c('1/22', '2/22',
#                                                '1/23', '2/23',
#                                                '1/24', '2/24'))
#                                 
# 
# #writexl::write_xlsx(tempo_profissionais, "tempo_profissionais.xlsx")
# 
# tempo_profissionais <- tempo_profissionais %>% 
#                             mutate(id = case_when(batch == '1/22' ~ 1, 
#                                                    batch == '2/22' ~ 2,
#                                                    batch == '1/23' ~ 3,
#                                                    batch == '2/23' ~ 4,
#                                                    batch == '1/24' ~ 5,
#                                                    batch == '2/24' ~ 6))
                                               

demanda_profissionais %>% 
  ggplot(aes(x = ano, y = total, 
             fill = Profissional, group = 1)) + 
  geom_col(group = 1, position = "dodge") + facet_grid(~regiao) + 
  theme_minimal() + theme(text = element_text(size = 22)) + xlab("Ano") + 
  ylab("Total de profissionais em ETI40") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_discrete(labels = c("Enfermeiro AB", "M?dico AB", "M?dico AS")) + 
  theme(legend.position = 'bottom')



# Oferta de profissionais -------------------------------------------------

oferta_consolidada <- read_excel("~/GitHub/prophet_gestantes/bases/oferta consolidada.xlsx")

oferta_consolidada <- oferta_consolidada %>%  
  mutate(ano = year(mes_ano)) %>% 
  filter(ano > 2021) %>% 
  group_by(macrorregiao, ano, nivel_atencao, 
           categoria, cen?rio) %>% 
  summarise(total = sum(qtd_mensal)) %>% 
  ungroup() %>% 
  select(ano, macrorregiao, categoria, total, cen?rio) %>% 
  rename(regiao = macrorregiao, Profissional = categoria)


# comparando oferta e demanda ---------------------------------------------

demanda_oferta <- rbind(oferta_consolidada, demanda_profissionais)


demanda_oferta %>% 
  filter(Profissional == "enf_ab") %>% 
  ggplot(aes(x = ano, y = total, col = cen?rio)) + geom_line(size = 1) + geom_point() +
  geom_label(aes(label = round(total), fill = cen?rio), colour = "white", fontface = "bold", size = 8) + 
  theme_minimal() + facet_wrap(~regiao, nrow = 2, ncol = 3) + scale_x_continuous(breaks = seq(2022, 2024, 1)) +
  theme(legend.position = 'bottom') + theme(text = element_text(size = 23)) +
  guides(col=guide_legend(nrow=2,byrow=TRUE)) + 
  ggtitle("Demanda vs Oferta - Enfermeiro (AB)")



demanda_oferta %>% 
  filter(Profissional == "medico_ab") %>% 
  ggplot(aes(x = ano, y = total, col = cen?rio)) + geom_line(size = 1) + geom_point() +
  geom_label(aes(label = round(total), fill = cen?rio), colour = "white", fontface = "bold", size = 8) + 
  theme_minimal() + facet_wrap(~regiao, nrow = 2, ncol = 3) + scale_x_continuous(breaks = seq(2022, 2024, 1)) +
  theme(legend.position = 'bottom') + theme(text = element_text(size = 23)) +
  guides(col=guide_legend(nrow=2,byrow=TRUE)) +
  ggtitle("Demanda vs Oferta - M?dico (AB)")



demanda_oferta %>% 
  filter(Profissional == "medico_esp") %>% 
  ggplot(aes(x = ano, y = total, col = cen?rio)) + geom_line(size = 1) + geom_point() +
  geom_label(aes(label = round(total), fill = cen?rio), colour = "white", fontface = "bold", size = 8) + 
  theme_minimal() + facet_wrap(~regiao, nrow = 2, ncol = 3) + scale_x_continuous(breaks = seq(2022, 2024, 1)) +
  theme(legend.position = 'bottom') + theme(text = element_text(size = 23)) +
  guides(col=guide_legend(nrow=2,byrow=TRUE)) + 
  ggtitle("Demanda vs Oferta - M?dico (AS)")




# calculando deficits -----------------------------------------------------

deficit <- demanda_oferta %>% 
  mutate(status = case_when(cen?rio == "Oferta - Cen?rio 1 - constante" ~ "oferta1", 
                            cen?rio == "Oferta - Cen?rio 2 - aumento de 5% ao ano" ~ "oferta2",
                            cen?rio == "Oferta - Cen?rio 3 - altera??o para os n?veis pr?-pandemia (12/2019)" ~ "oferta3",
                            cen?rio == "demanda" ~ "demanda"
                            )) %>% 
  select(-cen?rio) %>% 
  spread(status, total) %>% 
  group_by(regiao, Profissional) %>% 
  summarize_all(mean) %>% 
  mutate(resultado1 = oferta1 - demanda,
         resultado2 = oferta2 - demanda,
         resultado3 = oferta3 - demanda) 


writexl::write_xlsx(deficit, "deficit.xlsx")


deficit_gather <- deficit %>%  
                  select(ano, regiao, Profissional, 
                         resultado1, resultado2, resultado3) %>% 
                  gather(key = cenario, value = "resultado", 
                         4:6)
  

deficit_gather %>% 
  ggplot(aes(x = ano, y = resultado, fill = regiao)) + geom_col(position = "dodge") + 
  facet_grid(~Profissional) + coord_flip() + theme_minimal()
