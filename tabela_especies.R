# Pacotes ----

library(readxl)

library(tidyverse)

library(gt)

library(rmarkdown)

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

# Tabela ----

## Espécies ----

sps <- especies |>
  dplyr::filter(Ordem == "Anura" &
                  !Epípeto %in% c("natalensis", "mystaceus") &
                  Gênero != "Frostius" &
                  Família != "Hylidae") |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(`Unidade Amostral`, Espécie)) |>
  dplyr::pull(Espécie) |>
  unique()

sps

## Estatísticas de abundância ----

tabela <- especies |>
  dplyr::filter(Ordem == "Anura" &
                  !Epípeto %in% c("natalensis", "mystaceus") &
                  Gênero != "Frostius" &
                  Família != "Hylidae") |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(`Unidade Amostral`, Espécie)) |>
  dplyr::left_join(especies |>
                     dplyr::select(Família, Espécie),
                   by = "Espécie") |>
  dplyr::relocate(Família, .before = Espécie) |>
  dplyr::distinct() |>
  dplyr::mutate(Espécie = dplyr::case_match(Espécie,
                                            "Adenomera hylaedactyla" ~ "Adenomera aff. hylaedactyla",
                                            .default = Espécie),
                Autoria = dplyr::case_match(Espécie,
                                             "Pristimantis ramagii" ~ " (Boulenger, 1888)",
                                            "Adenomera aff. hylaedactyla" ~ " (Cope, 1868)",
                                            "Rhinella hoogmoedi" ~ " Caramaschi and Pombal, 2006",
                                            "Rhinella granulosa" ~ " (Spix, 1824)",
                                            "Leptodactylus troglodytes" ~ " Lutz, 1926",
                                            "Dryadobates alagoanus" ~ " (Bokermann, 1967)",
                                            "Adelophryne nordestina" ~ " Lourenço-de-Moraes, Lisboa, Drummond, Moura, Moura, Lyra, Guarnieri, Mott, Hoogmoed, and Santana, 2021",
                                            "Physalaemus cuvieri" ~ " Fitzinger, 1826",
                                            "Elachistocleis cesari" ~ " (Miranda-Ribeiro, 1920)",
                                            "Rhinella crucifer" ~" (Wied-Neuwied, 1821)"),
                Espécie = paste0("<i>", Espécie, "</i>"),
                Espécie = Espécie |> stringr::str_replace_all(c(" aff " = "</i> aff. <i>",
                                                                " aff. " = "</i> aff. <i>",
                                                                " gr " = "</i> gr. <i>",
                                                                " gr. " = "</i> gr. <i>",
                                                                " cf " = "</i> cf. <i>",
                                                                " cf. " = "</i> cf. <i>"))) |>
  dplyr::arrange(`Unidade Amostral`, Família) |>
  dplyr::rename("Sampling Unit" = `Unidade Amostral`,
                "Family" = Família,
                "Species" = Espécie,
                "Abundance" = Abundância) |>
  tidyr::unite(col = "Species",
               c(3, 5),
               sep = " ")

tabela

## Tabela gt ----

tabela_gt <- tabela |>
  gt() |>
  fmt_markdown(columns = Species) |>
  cols_align(align = "center",
             columns = everything()) |>
  cols_width(Species ~ px(300)) |>
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              table_body.hlines.color = "transparent",
              table.border.bottom.color = "black",
              table.font.color = "black",
              table.font.size = 12,
              table.font.names = "Arial",
              table.background.color = "white")

tabela_gt

## Exportando a tabela ----

tabela_gt |>
  gt::gtsave("tabela_especies.html")

rmarkdown::pandoc_convert("tabela_especies.html",
                          to = "docx",
                          output = "tabela_especies.docx")

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


