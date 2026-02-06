# Pacotes ----

library(coiR)

library(writexl)

# lista de diretórios -----

diretorios <- paste0("./imagens de dossel/",
                     list.files(path = "./imagens de dossel"))

diretorios

# Calculando índice de abertura de dossel ----

indice <- c()

trilha <- c()

parcela <- c()

campanha <- c()

calcular_indice <- function(diretorio){

  imagens <- list.files(path = diretorios[1])

  indice_calcular <- function(imagens){

    raster <- terra::rast(imagens)

    indice_dossel <- raster |>
      coiR::coir_crop(plot = FALSE) |>
      coiR::coir_binarize(plot = FALSE) |>
      coiR::coir_index(round = 5)

  }

  purrr::map(indice_calcular, imagens)

  indice <<- c(indice_dossel, indice)

}
