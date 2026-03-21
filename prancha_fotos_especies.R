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

  foto_importada <- terra::rast(foto) |>
    terra::flip(direction = "vertical")

  nome_sps <- foto |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_remove(".jpg|.jpeg|.JPG") |>
    stringr::str_extract("\\w+$")

  assign(paste0("foto_", nome_sps),
         foto_importada,
         envir = globalenv())

}

purrr::map(foto, importar_fotos)

## Visualizando ----

### Lista das fotos ----

fotos_unidas <- ls(pattern = "^foto_") |>
  rev() |>
  mget(envir = globalenv())

fotos_unidas

### Gráfico ----

visualizar_fotos <- function(fotos_unidas){

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = fotos_unidas) +
    coord_sf(expand = FALSE) +
    coord_equal() +
    theme_void()

  print(ggplt)

}

purrr::map(fotos_unidas, visualizar_fotos)

# Prancha ----

## Nome das fotos na lista ----

nome_especies <- c("ramagii",
                   "hylaedactyla",
                   "hoogmoedi")

nome_especies

## Criando os ggplots ----

criando_ggplots <- function(fotos_unidas, nome_especies){

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = fotos_unidas) +
    coord_sf(expand = FALSE) +
    coord_equal() +
    theme_void()

  assign(paste0("ggplot_", nome_especies),
         ggplt,
         envir = globalenv())

}

purrr::map2(fotos_unidas, nome_especies, criando_ggplots)

## Lista dos ggplots ----

lista_ggplots <- ls(pattern = "^ggplot_") |>
  rev() |>
  mget(envir = globalenv())

lista_ggplots

## Fotos unidas ----

