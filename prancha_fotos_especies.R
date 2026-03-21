# Pacotes ---

library(terra)

library(tidyverse)

library(tidyterra)

library(patchwork)

# Fotos ----

## Lista de fotos ----

foto <- list.files(pattern = "^foto_")

foto

## Importando ----

importar_fotos <- function(foto){

  foto_importada <- terra::rast(foto)

  nome_sps <- foto |>
    stringr::str_replace("_", " ") |>
    stringr::str_remove(".jpg|.jpeg|.JPG") |>
    stringr::str_extract("\\w+$")

  assign(paste0("foto_", nome_sps),
         foto_importada,
         envir = globalenv())

}

purrr::map(foto, importar_fotos)
