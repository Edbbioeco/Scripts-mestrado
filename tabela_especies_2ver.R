# Pacotes ----

library(readxl)

library(tidyverse)

library(tools)

library(flextable)

# Dados ----

## Importar ----

comp <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/levantamento_anuros.xlsx")

## Visualizar ----

comp

comp |> dplyr::glimpse()

# Imagens das espécies ----

## Lista das espécies ----

sps <- comp |>
  dplyr::filter(Ordem == "Anura" &
                  !Epípeto %in% c("natalensis", "mystaceus") &
                  Gênero != "Frostius" &
                  Família != "Hylidae") |>
  dplyr::pull(Espécie) |>
  unique()

sps

## Imagens originais ----


imagens <- paste0("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/imagens_especies/",
                  sps,
                  ".",
                  list.files(path = "C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/imagens_especies/",
                             pattern = ".jpeg|.JPG") |>
                    tools::file_ext() |>
                    stringr::str_to_lower())

imagens

## Imagens cortadas ----

imagens_cortadas <- list.files(path = "C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/imagens_especies/",
                               pattern = ".tif$",
                               full.names = TRUE)

imagens_cortadas

# Tabela ----

## Tratar ----

comp_trat <- comp |>
  dplyr::filter(Ordem == "Anura" &
                  !Epípeto %in% c("natalensis", "mystaceus") &
                  Gênero != "Frostius" &
                  Família != "Hylidae") |>
  dplyr::mutate(Espécie = dplyr::case_match(
    Espécie,
    "Pristimantis ramagii" ~ "PR",
    "Adenomera hylaedactyla" ~ "AH",
    "Rhinella hoogmoedi" ~ "RH",
    "Rhinella granulosa" ~ "RG",
    "Leptodactylus troglodytes" ~ "LT",
    "Dryadobates alagoanus" ~ "DA",
    "Adelophryne nordestina" ~ "AN",
    "Physalaemus cuvieri" ~ "PC",
    "Elachistocleis cesari" ~ "EC",
    "Rhinella crucifer" ~ "RC"
  )) |>
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

## Imagens originais ----

### Criar a tabela ----

comp_flex <- comp_trat |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 1, j = 2) |>
  flextable::italic(part = "header", j = 2:11) |>
  flextable::fontsize(size = 10, part = "all") |>
  flextable::color(color = "black", part = "all") |>
  flextable::add_header_row(values = rep(NA, 11),
                            top = TRUE) |>
  flextable::compose(i = 1,
                     j = 2:11,
                     part = "header",
                     value = flextable::as_paragraph(

                       flextable::as_image(src = imagens,
                                           width = 0.45,
                                           height = 0.45)

                     ))

comp_flex

### Salvar a tabela ----

comp_flex |> flextable::save_as_docx(path = "C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/tabela_abundancia_cap2_2ver.docx")

comp_flex |> flextable::save_as_image(path = "C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/tabela_abundancia_cap2_2ver.png")
