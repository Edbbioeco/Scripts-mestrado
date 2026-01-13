# PAcotes----

library(readxl)

library(tidyverse)

library(magrittr)

library(writexl)

# Dados ----

## Importando ----

especies <- readxl::read_xlsx("levantamento_anuros.xlsx")

## Visualizando ----

especies

especies |> dplyr::glimpse()

## Tratando ----

especies %<>%
  dplyr::mutate(`Segmento da parcelas` = `Segmento da parcelas` |>
                  as.character())

especies |> dplyr::glimpse()

# Matriz de composição ----

## Criando a matriz de composição ----

especies_alfa <- especies |>
  dplyr::filter(Ordem == "Anura" &
                  !Epípeto == "natalensis" &
                  Gênero != "Frostius" &
                  Família != "Hylidae") |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(`Unidade Amostral`, Espécie, Campanha)) |>
  dplyr::summarise(Abundância = Abundância |> max(),
                   .by = c(`Unidade Amostral`, Espécie)) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Abundância,
                     values_fill = 0)

especies_alfa

## Exportando ----

especies_alfa |>
  writexl::write_xlsx("matriz_composicao.xlsx")

