# Pacotes ----

library(readxl)

library(tidyverse)

# Dados ----

## Importando ----

especies <- readxl::read_xlsx("levantamento_anuros.xlsx")

## Visualizando ----

especies

especies |> dplyr::glimpse()

## Tratando ----

especies <- especies |>
  dplyr::mutate(`Segmento da parcelas` = `Segmento da parcelas` |>
                  as.character())

especies |> dplyr::glimpse()
