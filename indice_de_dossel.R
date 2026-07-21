# Pacotes ----

library(tidyverse)

library(coiR)

library(writexl)

# lista de diretórios -----

## Diretorios ----

diretorios <- paste0("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/imagens de dossel/",
                     list.files(path = "C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/imagens de dossel"))

diretorios

## Imagens ----

imagens <- list.files(path = diretorios,
                      full.names = TRUE)

imagens

# Calculando índice de abertura de dossel ----

## Loop ----

df_dossel <- purrr::map_dfr(
  .x = imagens,
  purrr::in_parallel(.f = \(imagens){

             raster <- terra::rast(imagens)

             indice_dossel <- raster |>
               coiR::coir_crop(plot = FALSE) |>
               coiR::coir_binarize(threshold = 0.75,
                                   plot = FALSE) |>
               coiR::coir_index(round = 5)

             trilha_dossel <- dplyr::case_when(
               imagens |>
                 stringr::str_detect("/T1") ~ "1",
               imagens |>
                 stringr::str_detect("/T2") ~ "2",
               imagens |>
                 stringr::str_detect("/T3") ~ "3",
               imagens |>
                 stringr::str_detect("/R") ~ "Ripária")

             parcela_dossel <- dplyr::case_when(
               imagens |>
                 stringr::str_detect("P1|R1") ~ "1",
               imagens |>
                 stringr::str_detect("P2|R2") ~ "2",
               imagens |>
                 stringr::str_detect("P3") ~ "3",
               imagens |>
                 stringr::str_detect("P4") ~ "4")

             campanha_dossel <- dplyr::case_when(
               imagens |>
                 stringr::str_detect("C1") ~ "1",
               imagens |>
                 stringr::str_detect("C2") ~ "2",
               imagens |>
                 stringr::str_detect("C3") ~ "3")

             ponto_dossel <- dplyr::case_when(
               imagens |>
                 stringr::str_detect("P000") ~ "P000",
               imagens |>
                 stringr::str_detect("P050") ~ "P050",
               imagens |>
                 stringr::str_detect("P100") ~ "P100",
               imagens |>
                 stringr::str_detect("P150") ~ "P150",
               imagens |>
                 stringr::str_detect("P200") ~ "P200",
               imagens |>
                 stringr::str_detect("P250") ~ "P250")

             tibble::tibble(Trilha = trilha_dossel,
                            Parcela = parcela_dossel,
                            Campanha = campanha_dossel,
                            Pontos = ponto_dossel,
                            Índice = indice_dossel)

           }),
           .progress = TRUE)

## Data frame final ----

df_dossel |> as.data.frame()

## Histograma ----

df_dossel |>
  ggplot(aes(Índice)) +
  geom_histogram(color = "black", binwidth = 0.0075) +
  facet_wrap(~Trilha)

# Exportando os dados ----

df_dossel |>
  writexl::write_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/dados_indice_dossel_coir.xlsx")
