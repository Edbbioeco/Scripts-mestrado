# Pacotes ----

library(coiR)

library(writexl)

# lista de diretórios -----

diretorios <- paste0("./imagens de dossel/",
                     list.files(path = "./imagens de dossel"))

diretorios

# Calculando índice de abertura de dossel ----

## Loop ----

indice <- c()

trilha <- c()

parcela <- c()

campanha <- c()

calcular_indice <- function(diretorios){

  imagens <- paste0(diretorios,
                    "/",
                    list.files(path = diretorios))

  indice_calcular <- function(imagens){

    raster <- terra::rast(imagens)

    indice_dossel <- raster |>
      coiR::coir_crop(plot = FALSE) |>
      coiR::coir_binarize(plot = FALSE) |>
      coiR::coir_index(round = 5)

    indice <<- c(indice, indice_dossel)

  }

  purrr::map(imagens, indice_calcular)

  trilha_dossel <- dplyr::case_when(diretorios |>
                                      stringr::str_detect("T1") ~ "1",
                                    diretorios |>
                                      stringr::str_detect("T2") ~ "2",
                                    diretorios |>
                                      stringr::str_detect("R") ~ "Ripária")

  trilha <- c(trilha, trilha_dossel)

  parcela_dossel <- dplyr::case_when(diretorios |>
                                       stringr::str_detect("P1|R1") ~ "1",
                                     diretorios |>
                                       stringr::str_detect("P2|R2") ~ "2",
                                     diretorios |>
                                       stringr::str_detect("P3") ~ "3",
                                     diretorios |>
                                       stringr::str_detect("P4") ~ "4")

  parcela <- c(parcela, parcela_dossel)

  campanha_dossel <- dplyr::case_when(imagens |>
                                        stringr::str_detect("C1") ~ "1",
                                      imagens |>
                                        stringr::str_detect("C2") ~ "2",
                                      imagens |>
                                        stringr::str_detect("C3") ~ "3")

  campanha <- c(campanha, campanha_dossel)

  df_dossel <<- tibble::tibble(Trilha = trilha,
                               Parcela = parcela,
                               Campanha = campanha,
                               Índice = indice)

}

purrr::map(.x = diretorios,
           .f = calcular_indice)

## Data frame final ----

df_dossel |> as.data.frame()
