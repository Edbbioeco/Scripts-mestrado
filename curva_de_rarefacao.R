# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggview)

# Dados ----

## Importar ----

comp <- readxl::read_xlsx("levantamento_anuros.xlsx")

## Visualizar ----

comp

comp |> dplyr::glimpse()

## Tratar ----

comp_trat <- comp |>
  dplyr::filter(Ordem == "Anura" &
                  !Epípeto == "natalensis" &
                  Gênero != "Frostius" &
                  Família != "Hylidae" &
                  `Unidade Amostral` != "T1P1") |>
  dplyr::summarise(Abundância = Abundância |> max(),
                   .by = c(Espécie, `Unidade Amostral`, Campanha)) |>
  dplyr::mutate(`Unidade Amostral` = paste0(Campanha, " ", `Unidade Amostral`)) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Abundância,
                     values_fill = 0)

comp_trat
