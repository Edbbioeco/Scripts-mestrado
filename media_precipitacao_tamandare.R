# Pacotes ----

library(readxl)

library(tidyverse)

library(writexl)

# Dados ----

## Importando ----

prec <- readxl::read_xlsx("dados_apac_pluv.xlsx")

## Visualizando ----

prec

prec |> dplyr::glimpse()

# Média de precipitação ----

## Tratando ----

prec_trat <- prec |>
  tidyr::pivot_longer(cols = Janeiro:Dezembro,
                      names_to = "Mês",
                      values_to = "Precipitação")

prec_trat
