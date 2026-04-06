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

## média anual ----

### Calculando ----

prec_trat |>
  dplyr::summarise(Media = mean(Precipitação, na.rm = TRUE),
                   .by = Ano)

### Exportando ----

prec_trat |>
  dplyr::summarise(Media = mean(Precipitação, na.rm = TRUE),
                   .by = Ano) |>
  writexl::write_xlsx("media_precipitacao_tamandare_anual.xlsx")

## Média mensal ----

### Calculando ----

prec_trat |>
  dplyr::summarise(Media = mean(Precipitação, na.rm = TRUE),
                   .by = Mês)

### Exportando ----

prec_trat |>
  dplyr::summarise(Media = mean(Precipitação, na.rm = TRUE),
                   .by = Mês) |>
  writexl::write_xlsx("media_precipitacao_tamandare_mensal.xlsx")
