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



# Estatísticas sobre as espécies ----

## Quantidade de espécies por família ----

especies |>
  dplyr::filter(Ordem == "Anura" &
                  !Epípeto %in% c("natalensis", "mystaceus") &
                  Gênero != "Frostius" &
                  Família != "Hylidae") |>
  dplyr::summarise(Número = Espécie |> dplyr::n_distinct(),
                   .by = Família) |>
  dplyr::arrange(Número |> dplyr::desc())

## Abundância máxima por família ----

especies |>
  dplyr::filter(Ordem == "Anura" &
                  !Epípeto %in% c("natalensis", "mystaceus") &
                  Gênero != "Frostius" &
                  Família != "Hylidae") |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(Família, Campanha)) |>
  dplyr::slice_max(Abundância,
                   n = 1,
                   by = Família) |>
  dplyr::arrange(Abundância |> dplyr::desc())

## Abundância máxima por espécie ----

especies |>
  dplyr::filter(Ordem == "Anura" &
                  !Epípeto %in% c("natalensis", "mystaceus") &
                  Gênero != "Frostius" &
                  Família != "Hylidae") |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(Espécie, Campanha)) |>
  dplyr::slice_max(Abundância,
                   n = 1,
                   by = Espécie) |>
  dplyr::arrange(Abundância |> dplyr::desc())
