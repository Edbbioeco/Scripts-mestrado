# Pacotes ----

library(readxl)

library(tidyverse)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("dados_pis_saltinho.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

# Tratando ----

## Criando um dataframe com os dados de PIS completos ----




