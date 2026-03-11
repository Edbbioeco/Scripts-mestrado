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

dados_pis <- tibble::tibble(ID = paste("0", 566:603))

dados_pis

dados_pis |> dplyr::glimpse()


