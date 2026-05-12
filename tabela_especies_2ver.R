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
                     values_from = Abundância) |>
  dplyr::rename("Adenomera aff. hylaedactyla" = `Adenomera hylaedactyla`)

comp_trat

## Tabela flextable ----

comp_flex <- comp_trat |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::italic(part = "header", j = c(2, 4:11)) |>
  flextable::compose(part = "header",
                     j = 3,
                     value = flextable::as_paragraph(
                       flextable::as_i("Adenomera"),
                       " aff. ",
                       flextable::as_i("hylaedactyla")
                     ))

comp_flex

## Salvar a tabela ----

comp_flex |> flextable::save_as_docx(path = "tabela_abundancia_cap2_2ver.docx")
