# Pacotes ----

library(coiR)

library(writexl)

# lista de diretórios -----

## Diretorios ----

diretorios <- paste0("./imagens de dossel/",
                     list.files(path = "./imagens de dossel"))

diretorios

## Imagens ----

imagens <- list.files(path = diretorios,
                      full.names = TRUE)

imagens

# Calculando índice de abertura de dossel ----

## Loop ----

calcular_indice <- function(imagens){

  raster <- terra::rast(imagens)

  indice_dossel <- raster |>
    coiR::coir_crop(plot = FALSE) |>
    coiR::coir_binarize(plot = FALSE) |>
    coiR::coir_index(round = 5)

  indice <<- c(indice, indice_dossel)

  trilha_dossel <- dplyr::case_when(imagens[1] |>
                                      stringr::str_detect("/T1") ~ "1",
                                    imagens[1] |>
                                      stringr::str_detect("/T2") ~ "2",
                                    imagens[1] |>
                                      stringr::str_detect("/T3") ~ "3",
                                    imagens[1] |>
                                      stringr::str_detect("/R") ~ "Ripária")

  trilha <<- c(trilha, trilha_dossel)

  parcela_dossel <- dplyr::case_when(imagens |>
                                       stringr::str_detect("P1|R1") ~ "1",
                                     imagens |>
                                       stringr::str_detect("P2|R2") ~ "2",
                                     imagens |>
                                       stringr::str_detect("P3") ~ "3",
                                     imagens |>
                                       stringr::str_detect("P4") ~ "4")

  parcela <<- c(parcela, parcela_dossel)

  campanha_dossel <- dplyr::case_when(imagens |>
                                        stringr::str_detect("C1") ~ "1",
                                      imagens |>
                                        stringr::str_detect("C2") ~ "2",
                                      imagens |>
                                        stringr::str_detect("C3") ~ "3")

  campanha <<- c(campanha, campanha_dossel)

  df_dossel <<- tibble::tibble(Trilha = trilha,
                               Parcela = parcela,
                               Campanha = campanha,
                               Índice = indice)

}

indice <- c()

trilha <- c()

parcela <- c()

campanha <- c()

purrr::map(.x = imagens,
           .f = calcular_indice)

## Data frame final ----

df_dossel |> as.data.frame()

# Exportando os dados ----

df_dossel |> writexl::write_xlsx("dados_indice_dossel_coir.xlsx")
