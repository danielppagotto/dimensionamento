# Carregando pacotes -----
library(tidyverse)
library(lubridate) 
library(modeltime)
library(tidymodels)
library(timetk)
library(readxl)

# Leitura dos dados ----

municipios_path <- "00_bases/municipio_regiao.csv"
nascimentos_path <- "00_bases/nascimentos.csv"
regioes_path <- "00_bases/regioes_nomes.csv"
macrorregiao_path <- "00_bases/municipios_macrorregiao_saude.csv"

municipio_regiao <- read_csv(municipios_path, 
                             col_types = cols(CO_MUNICIP = col_character(), 
                                              CO_REGSAUD = col_character())) %>% 
                            janitor::clean_names()

nascimentos <- read_csv(nascimentos_path) %>% 
               select(-`...1`) %>% janitor::clean_names() 

regioes_nomes <- read_csv(regioes_path, 
                          col_types = cols(CO_REGSAUD = col_character())) %>% 
                 janitor::clean_names() %>% 
                 select(co_regsaud, ds_nomepad)

municipios_macrorregiao_saude <- read_csv(macrorregiao_path) 

# Tratamentos iniciais -----

nascimentos <- nascimentos %>% 
                    mutate(data = substr(dtnasc, 3, 8))

nascimentos$data <- my(nascimentos$data)
                    

nascimentos_go <- nascimentos %>% 
                    filter(data < "2021-10-01") %>%
                    mutate(codmunres6 = as.numeric(substr(codmunres, 1, 6))) %>% 
                    left_join(municipios_macrorregiao_saude, by = c("codmunres6"="cod_municipio")) %>% 
                    group_by(macrorregiao, data) %>%
                    summarise(total = sum(contagem)) %>% 
                    filter(macrorregiao != "NA") %>% 
                    mutate(status = 'atual')

# Numero de nascimentos por macrorregiao de saude

nascimentos_go %>% 
  ggplot(aes(x = data, y = total, col = macrorregiao)) + geom_line() + 
  theme_minimal()


datas <- nascimentos_go %>% 
                ungroup() %>% 
                select(data) %>% 
                distinct(data) 

nascimentos_go %>% 
  ggplot(aes(x = data, y = total)) + geom_line(size = 0.60) + 
  theme_minimal() + facet_wrap(~macrorregiao)

# Plotando serie temporal por macrorregiao

nascimentos_go %>% 
  plot_time_series(data, total)


# Inicio de funcoes ----------------------------------------------
# funcao split ---------------------------------------------------

funcao_split <- function(nome_macrorregiao){
  
  nasc_macrorregiao <- 
    nascimentos_go %>% 
    filter(macrorregiao == nome_macrorregiao) %>% 
    ungroup() %>% 
    select(-macrorregiao)
  
  split_macrorregiao <- 
    time_series_split(
      nasc_macrorregiao,
      assess = "12 months",
      cumulative = TRUE
    )
  
  split_macrorregiao %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(data, total)
  
  return(split_macrorregiao)
  
}

# funcao treino modelos --------------------------------------------

treino <- function(splits){

  model_arima <- arima_reg() %>% 
    set_engine("auto_arima") %>% 
    fit(total ~ data, training(splits))
  
  model_prophet <- prophet_reg(seasonality_yearly = TRUE) %>%
    set_engine("prophet") %>% 
    fit(total ~ data, training(splits))
  
 # model_fit_ets <- exp_smoothing() %>%
   # set_engine(engine = "ets") %>%
   # fit(total ~ data, data = training(splits))
  
  model_tbl <- modeltime_table(
    model_arima,
    model_prophet,
    #model_fit_ets
  )
  
  calib_tbl <- model_tbl %>% 
    modeltime_calibrate(testing(splits))
  
  calib_tbl %>% modeltime_accuracy()
  
    return(calib_tbl)

}

# Funcao previsao

previsao <- function(modelo, split, atual){

  modeltime_forecast(object = modelo,
                     new_data = testing(split),
                     actual_data = atual) %>% 
                     plot_modeltime_forecast(.conf_interval_show = FALSE)

  future_forecast <- 
    modeltime_refit(object = modelo, atual) %>% 
    modeltime_forecast(h = "42 months",
                     actual_data = atual) 

  future_forecast %>% 
    plot_modeltime_forecast(.conf_interval_show = FALSE)
  
  return(future_forecast)

}

# Nascimentos futuros e atuais ----

nascimentos_futuros <- function(objeto, algoritmo, regiao){
    
    objeto %>% 
      filter(.model_id == algoritmo) %>% 
      mutate(mes_ano = format(.index, "%Y-%m")) %>% 
      mutate(ano = year(.index)) %>% 
      group_by(mes_ano, ano) %>% 
      summarise(total = sum(.value)) %>% 
      mutate(tipo = "previsto")%>% 
      mutate(regiao = regiao)  
}

nascimentos_atuais <- function(objeto, regiao){

  objeto %>% 
    filter(.index > "2015-01-01") %>% 
    filter(.key == "actual") %>% 
    mutate(mes_ano = format(.index, "%Y-%m")) %>% 
    mutate(ano = year(.index)) %>% 
    group_by(mes_ano, ano) %>% 
    summarise(total = sum(.value)) %>% 
    mutate(tipo = "atual") %>% 
    mutate(regiao = regiao)
}

# Funcao modelagem ----
# Juntando todas a funcoes 

modelagem <- function(regiao){

  splits_regiao <- funcao_split(regiao)
  calib <- treino(splits = splits_regiao)
  
  resumo_modelos <- calib %>% 
                      modeltime_accuracy() 
  
  flag <- calib %>% 
    modeltime_accuracy() %>% 
    slice(which.min(mape)) %>% 
    mutate(melhor = case_when(.model_id == 1 ~ 1,
                              .model_id == 2 ~ 2))
                              #.model_id == 3 ~ 3)) 

  melhor_modelo <- calib[[5]][[flag$melhor]]

  grafico_melhor <- melhor_modelo %>% 
    ggplot(aes(x = data)) + geom_line(aes(y = .actual), col = "blue") +
    geom_line(aes(y = .prediction), col = "red") + theme_minimal()

  nasc_atuais <- nascimentos_go %>% 
    filter(macrorregiao == regiao) %>% 
    ungroup() %>% 
    select(-macrorregiao)

  regiao_future <- 
    previsao(modelo = calib, split = splits_regiao,
             atual = nasc_atuais)

  grafico_previsao <- regiao_future %>% 
    filter(.model_id == flag$.model_id & .model_desc == "ACTUAL") %>% 
    filter(.index > "2019-01-01") %>% 
    ggplot(aes(x = .index, y = .value, col = .key)) + geom_line() +
    theme_minimal()

  nascimentos_futuros_regiao <- 
    nascimentos_futuros(objeto = regiao_future,
                        algoritmo = flag$.model_id, 
                        regiao = regiao)

  nascimentos_atuais_regiao <- 
    nascimentos_atuais(regiao_future, regiao = regiao)

  lista <- 
    list(regiao_future,
         resumo_modelos,
         melhor_modelo,
         grafico_melhor,
         nascimentos_futuros_regiao,
         nascimentos_atuais_regiao,
         grafico_previsao
       )
  
  return(lista)
  
}

# Fim das funcoes ---------------------------------------------------------

ne_split <- funcao_split("Macrorregião Nordeste")
treino_ne <- treino(ne_split)

# Aplicando para cada regiao ----

regiao_centro_norte <- modelagem("Macrorregião Centro-Norte")
regiao_centro_oeste <- modelagem("macrorregião Centro-Oeste")
regiao_centro_sudeste <- modelagem("Macrorregião Centro Sudeste")
regiao_nordeste <- modelagem("Macrorregião Nordeste")
regiao_sudoeste <- modelagem("Macrorregião Sudoeste")

# Acessando metricas

metricas_centro_norte <- regiao_centro_norte[[2]]
metricas_centro_oeste <- regiao_centro_oeste[[2]]
metricas_centro_sudeste <- regiao_centro_sudeste[[2]]
metricas_nordeste <- regiao_nordeste[[2]]
metricas_sudoeste <- regiao_sudoeste[[2]]

# Acessando as previsoes 

prev_centro_norte <- regiao_centro_norte[[5]]
prev_centro_oeste <- regiao_centro_oeste[[5]]
prev_centro_sudeste <- regiao_centro_sudeste[[5]]
prev_nordeste <- regiao_nordeste[[5]]
prev_sudoeste <- regiao_sudoeste[[5]]



nascimentos_previsao <- rbind(prev_centro_norte,
                              prev_centro_oeste,
                              prev_centro_sudeste,
                              prev_nordeste,
                              prev_sudoeste)
  
nascimentos_previsao$mes_ano_data <- ym(nascimentos_previsao$mes_ano)

nascimentos_previsao %>% 
  ggplot(aes(x = mes_ano_data, y = total, col = regiao)) + geom_line() +
  theme_minimal() + facet_wrap(~regiao, scales = "free")

nascimentos_previsao %>% 
  filter(ano > 2021 & ano < 2025) %>% 
  group_by(ano, regiao) %>% 
  summarise(total = sum(total)) %>% 
  ggplot(aes(x = ano, y = total, col = regiao)) + geom_line() +
  geom_label(aes(label = total)) +
  theme_minimal() + facet_wrap(~regiao, scales = "free") 

teste<- nascimentos_previsao %>% 
  filter(ano > 2021 & ano < 2025) %>% 
  group_by(ano, regiao) %>% 
  summarise(total = sum(total))

# # write.csv(nascimentos_previsao, "nascimentos_previsao.csv")
# # writexl::write_xlsx(nascimentos_previsao, "nascimentos_previsao.xlsx")

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


# writexl::write_xlsx(deficit, "deficit.xlsx")


deficit_gather <- deficit %>%  
                  select(ano, regiao, Profissional, 
                         resultado1, resultado2, resultado3) %>% 
                  gather(key = cenario, value = "resultado", 
                         4:6)
  

deficit_gather %>% 
  ggplot(aes(x = ano, y = resultado, fill = regiao)) + geom_col(position = "dodge") + 
  facet_grid(~Profissional) + coord_flip() + theme_minimal()
