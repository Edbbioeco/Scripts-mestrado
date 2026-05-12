# Pacotes ----

library(readxl)

library(tidyverse)

library(flextable)

# Dados ----

## Importar ----

comp <- readxl::read_xlsx("levantamento_anuros.xlsx")

## Visualizar ----

comp

comp |> dplyr::glimpse()

# Tabela ----

## Tratar ----

comp_trat <- comp |>
  dplyr::filter(Ordem == "Anura" &
                  !Epípeto %in% c("natalensis", "mystaceus") &
                  Gênero != "Frostius" &
                  Família != "Hylidae") |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(`Unidade Amostral`, Espécie, Campanha)) |>
  tidyr::pivot_wider(names_from = `Unidade Amostral`,
                     values_from = Abundância,
                     values_fill = 0) |>
  tidyr::pivot_longer(names_to = "Unidade Amostral",
                      values_to = "Abundância",
                      cols = 3:13) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Abundância,
                     values_fill = 0) |>
  tidyr::pivot_longer(names_to = "Espécie",
                      values_to = "Abundância",
                      cols = 3:12) |>
  dplyr::summarise(Abundância = Abundância |>
                     stringr::str_c(collapse = ", "),
                   .by = c(`Unidade Amostral`, Espécie)) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Abundância)

comp_trat

## Tabela flextable ----

comp_flex <- comp_trat |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::italic(part = "header", j = 2:11)

comp_flex
