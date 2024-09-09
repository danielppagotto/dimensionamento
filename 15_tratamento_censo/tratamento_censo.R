
library(tidyverse)
library(readxl)

setwd("~/GitHub/dimensionamento/15_tratamento_censo")

pop_censo <- read_excel("populacao_censo_sexo_idade.xlsx")


pop_tratada  <- 
  pop_censo |> 
  gather(key = "categoria",
         value = "total", 4:45) |>
  separate(categoria, 
           into = c("categoria", "sexo"), sep = "_") |> 
  mutate(sexo = if_else(sexo == 'h', "Masculino", "Feminino"))
  

writexl::write_xlsx(pop_tratada, "populacao_censo22.xlsx")
write.csv(pop_tratada, "populacao_censo22.csv")
