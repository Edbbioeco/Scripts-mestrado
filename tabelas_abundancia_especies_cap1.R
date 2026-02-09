# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(flextable)

# Dados ----

## Importando ----

especies <- readxl::read_xlsx("levantamento_anuros.xlsx")

### Visualizando ----

especies

especies |> dplyr::glimpse()

## Tratando ----

especies %<>%
  dplyr::mutate(`Segmento da parcelas` = `Segmento da parcelas` |>
                  as.character())

especies |> dplyr::glimpse()

# Abundância das espécies ----

## Tabela de abundância ----

abund <- especies |>
  dplyr::filter(`Unidade Amostral` != "T1P1" &
                  Espécie %in% c("Pristimantis ramagii",
                                  "Adenomera hylaedactyla",
                                  "Rhinella hoogmoedi")) |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(Espécie, `Unidade Amostral`, Campanha)) |>
  dplyr::summarise(Abundância = Abundância |> max(),
                   .by = c(Espécie, `Unidade Amostral`)) |>
  dplyr::arrange(Espécie |> forcats::fct_relevel(c("Pristimantis ramagii",
                                                   "Adenomera hylaedactyla",
                                                   "Rhinella hoogmoedi")),
                 `Unidade Amostral`)

abund

## Abundância de cada espécie ----

### Total ----

abund |>
  dplyr::summarise(Abundância = Abundância |> sum())

### Por espécie ----

abund |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = Espécie)

## Abundância por campanha ----

# Flextable ----

## Criando a tabela ----

especies_abu_flex <- abund |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 2, j = 1) |>
  flextable::width(width = 2, j = 2) |>
  flextable::width(width = 1.25, j = 3) |>
  flextable::italic(j = 1, part = "body")

especies_abu_flex

## Exportando ----

especies_abu_flex |>
  flextable::save_as_docx(path = "tabela_especies_cap1.docx")
