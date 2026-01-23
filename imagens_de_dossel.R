# Pacotes ----

library(crayon)

library(terra)

library(tidyverse)

library(tidyterra)

library(hemispheR)

library(writexl)

# T1P1 ----

## Campanha 1 ----

## Campanha 2 ----

## Campanha 3 ----

## Unindo ----

# T1P2 ----

## Campanha 1 ----

lista_t1p2_c1 <- list.files(path = "./imagens de dossel/T1P2",
                            pattern = "C1.jpg")

lista_t1p2_c1

vetor_t1p2_c1 <- c()

fn_t1p2_c1 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T1P2/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T1P2/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t1p2_c1 <<- c(vetor_t1p2_c1, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t1p2_c1, fn_t1p2_c1)

vetor_t1p2_c1

t1p2_campanha1 <- tibble::tibble(`Índice de abetura` = vetor_t1p2_c1,
                                 Parcela = "T1P2",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 1")

t1p2_campanha1

## Campanha 2 ----

lista_t1p2_c2 <- list.files(path = "./imagens de dossel/T1P2",
                            pattern = "C2.jpg")

lista_t1p2_c2

vetor_t1p2_c2 <- c()

fn_t1p2_c2 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T1P2/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T1P2/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t1p2_c2 <<- c(vetor_t1p2_c2, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t1p2_c2, fn_t1p2_c2)

vetor_t1p2_c2

t1p2_campanha2 <- tibble::tibble(`Índice de abetura` = vetor_t1p2_c2,
                                 Parcela = "T1P2",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 2")

t1p2_campanha2

## Campanha 3 ----

lista_t1p2_c3 <- list.files(path = "./imagens de dossel/T1P2",
                            pattern = "C3.jpg")

lista_t1p2_c3

vetor_t1p2_c3 <- c()

fn_t1p2_c3 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T1P2/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T1P2/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t1p2_c3 <<- c(vetor_t1p2_c3, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t1p2_c3, fn_t1p2_c3)

vetor_t1p2_c3

t1p2_campanha3 <- tibble::tibble(`Índice de abetura` = vetor_t1p2_c3,
                                 Parcela = "T1P2",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 3")

t1p2_campanha3

## Unindo ----

df_t1p2 <- ls(pattern = "t1p2_campanha") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

df_t1p2

# T1P3 ----

## Campanha 1 ----

lista_t1p3_c1 <- list.files(path = "./imagens de dossel/T1P3",
                            pattern = "C1.jpg")

lista_t1p3_c1

vetor_t1p3_c1 <- c()

fn_t1p3_c1 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T1P3/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T1P3/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t1p3_c1 <<- c(vetor_t1p3_c1, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t1p3_c1, fn_t1p3_c1)

vetor_t1p3_c1

t1p3_campanha1 <- tibble::tibble(`Índice de abetura` = vetor_t1p3_c1,
                                 Parcela = "T1P3",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 1")

t1p3_campanha1

## Campanha 2 ----

lista_t1p3_c2 <- list.files(path = "./imagens de dossel/T1P3",
                            pattern = "C2.jpg")

lista_t1p3_c2

vetor_t1p3_c2 <- c()

fn_t1p3_c2 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T1P3/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T1P3/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t1p3_c2 <<- c(vetor_t1p3_c2, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t1p3_c2, fn_t1p3_c2)

vetor_t1p3_c2

t1p3_campanha2 <- tibble::tibble(`Índice de abetura` = vetor_t1p3_c2,
                                 Parcela = "T1P3",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 2")

t1p3_campanha2

## Campanha 3 ----

lista_t1p3_c3 <- list.files(path = "./imagens de dossel/T1P3",
                            pattern = "C3.jpg")

lista_t1p3_c3

vetor_t1p3_c3 <- c()

fn_t1p3_c3 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T1P3/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T1P3/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t1p3_c3 <<- c(vetor_t1p3_c3, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t1p3_c3, fn_t1p3_c3)

vetor_t1p3_c3

t1p3_campanha3 <- tibble::tibble(`Índice de abetura` = vetor_t1p3_c3,
                                 Parcela = "T1P3",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 3")

t1p3_campanha3

## Unindo ----

df_t1p3 <- ls(pattern = "t1p3_campanha") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

df_t1p3

# T1P4 ----

## Campanha 1 ----

lista_t1p4_c1 <- list.files(path = "./imagens de dossel/T1P4",
                            pattern = "C1.jpg")

lista_t1p4_c1

vetor_t1p4_c1 <- c()

fn_t1p4_c1 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T1P4/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T1P4/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t1p4_c1 <<- c(vetor_t1p4_c1, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t1p4_c1, fn_t1p4_c1)

vetor_t1p4_c1

t1p4_campanha1 <- tibble::tibble(`Índice de abetura` = vetor_t1p4_c1,
                                 Parcela = "T1P4",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 1")

t1p4_campanha1

## Campanha 2 ----

lista_t1p4_c2 <- list.files(path = "./imagens de dossel/T1P4",
                            pattern = "C2.jpg")

lista_t1p4_c2

vetor_t1p4_c2 <- c()

fn_t1p4_c2 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T1P4/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T1P4/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t1p4_c2 <<- c(vetor_t1p4_c2, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t1p4_c2, fn_t1p4_c2)

vetor_t1p4_c2

t1p4_campanha2 <- tibble::tibble(`Índice de abetura` = vetor_t1p4_c2,
                                 Parcela = "T1P4",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 2")

t1p4_campanha2

## Campanha 3 ----

## Unindo ----

df_t1p4 <- ls(pattern = "t1p4_campanha") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

df_t1p4

# T2P1 ----

## Campanha 1 ----

lista_t2p1_c1 <- list.files(path = "./imagens de dossel/T2P1",
                            pattern = "C1.jpg")

lista_t2p1_c1

vetor_t2p1_c1 <- c()

fn_t2p1_c1 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T2P1/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T2P1/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  message(paste0("analizando: ", x))

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t2p1_c1 <<- c(vetor_t2p1_c1, porcentagem_abertura_docel)

}

purrr::walk(lista_t2p1_c1, fn_t2p1_c1)

vetor_t2p1_c1

t2p1_campanha1 <- tibble::tibble(`Índice de abetura` = vetor_t2p1_c1,
                                 Parcela = "T2P1",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 1")

t2p1_campanha1

## Campanha 2 ----

lista_t2p1_c2 <- list.files(path = "./imagens de dossel/T2P1",
                            pattern = "C2.jpg")

lista_t2p1_c2

vetor_t2p1_c2 <- c()

fn_t2p1_c2 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T2P1/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T2P1/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t2p1_c2 <<- c(vetor_t2p1_c2, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t2p1_c2, fn_t2p1_c2)

vetor_t2p1_c2

t2p1_campanha2 <- tibble::tibble(`Índice de abetura` = vetor_t2p1_c2,
                                 Parcela = "T2P1",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 2")

t2p1_campanha2

## Campanha 3 ----

lista_t2p1_c3 <- list.files(path = "./imagens de dossel/T2P1",
                            pattern = "C3.jpg")

lista_t2p1_c3

vetor_t2p1_c3 <- c()

fn_t2p1_c3 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T2P1/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T2P1/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t2p1_c3 <<- c(vetor_t2p1_c3, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t2p1_c3, fn_t2p1_c3)

vetor_t2p1_c3

t2p1_campanha3 <- tibble::tibble(`Índice de abetura` = vetor_t2p1_c3,
                                 Parcela = "T2P1",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 3")

t2p1_campanha3

## Unindo ----

df_t2p1 <- ls(pattern = "t2p1_campanha") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

df_t2p1

# T2P2 ----

## Campanha 1 ----

lista_t2p2_c1 <- list.files(path = "./imagens de dossel/T2P2",
                            pattern = "C1.jpg")

lista_t2p2_c1

vetor_t2p2_c1 <- c()

fn_t2p2_c1 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T2P2/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T2P2/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t2p2_c1 <<- c(vetor_t2p2_c1, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t2p2_c1, fn_t2p2_c1)

vetor_t2p2_c1

t2p2_campanha1 <- tibble::tibble(`Índice de abetura` = vetor_t2p2_c1,
                                 Parcela = "T2P2",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 1")

t2p2_campanha1

## Campanha 2 ----

lista_t2p2_c2 <- list.files(path = "./imagens de dossel/T2P2",
                            pattern = "C2.jpg")

lista_t2p2_c2

vetor_t2p2_c2 <- c()

fn_t2p2_c2 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T2P2/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T2P2/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t2p2_c2 <<- c(vetor_t2p2_c2, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t2p2_c2, fn_t2p2_c2)

vetor_t2p2_c2

t2p2_campanha2 <- tibble::tibble(`Índice de abetura` = vetor_t2p2_c2,
                                 Parcela = "T2P2",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 2")

t2p2_campanha2

## Campanha 3 ----

lista_t2p2_c3 <- list.files(path = "./imagens de dossel/T2P2",
                            pattern = "C3.jpg")

lista_t2p2_c3

vetor_t2p2_c3 <- c()

fn_t2p2_c3 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T2P2/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T2P2/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t2p2_c3 <<- c(vetor_t2p2_c3, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t2p2_c3, fn_t2p2_c3)

vetor_t2p2_c3

t2p2_campanha3 <- tibble::tibble(`Índice de abetura` = vetor_t2p2_c3,
                                 Parcela = "T2P2",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 3")

t2p2_campanha3

## Unindo ----

df_t2p2 <- ls(pattern = "t2p2_campanha") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

df_t2p2

# T2P3 ----

## Campanha 1 ----

lista_t2p3_c1 <- list.files(path = "./imagens de dossel/T2P3",
                            pattern = "C1.jpg")

lista_t2p3_c1

vetor_t2p3_c1 <- c()

fn_t2p3_c1 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T2P3/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T2P3/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t2p3_c1 <<- c(vetor_t2p3_c1, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t2p3_c1, fn_t2p3_c1)

vetor_t2p3_c1

t2p3_campanha1 <- tibble::tibble(`Índice de abetura` = vetor_t2p3_c1,
                                 Parcela = "T2P3",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 1")

t2p3_campanha1

## Campanha 2 ----

lista_t2p3_c2 <- list.files(path = "./imagens de dossel/T2P3",
                            pattern = "C2.jpg")

lista_t2p3_c2

vetor_t2p3_c2 <- c()

fn_t2p3_c2 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T2P3/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T2P3/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t2p3_c2 <<- c(vetor_t2p3_c2, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t2p3_c2, fn_t2p3_c2)

vetor_t2p3_c2

t2p3_campanha2 <- tibble::tibble(`Índice de abetura` = vetor_t2p3_c2,
                                 Parcela = "T2P3",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 2")

t2p3_campanha2

## Campanha 3 ----

lista_t2p3_c3 <- list.files(path = "./imagens de dossel/T2P3",
                            pattern = "C3.jpg")

lista_t2p3_c3

vetor_t2p3_c3 <- c()

fn_t2p3_c3 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T2P3/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T2P3/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t2p3_c3 <<- c(vetor_t2p3_c3, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t2p3_c3, fn_t2p3_c3)

vetor_t2p3_c3

t2p3_campanha3 <- tibble::tibble(`Índice de abetura` = vetor_t2p3_c3,
                                 Parcela = "T2P3",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 3")

t2p3_campanha3

## Unindo ----

df_t2p3 <- ls(pattern = "t2p3_campanha") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

df_t2p3

# T2P4 ----

## Campanha 1 ----

lista_t2p4_c1 <- list.files(path = "./imagens de dossel/T2P4",
                            pattern = "C1.jpg")

lista_t2p4_c1

vetor_t2p4_c1 <- c()

fn_t2p4_c1 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T2P4/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T2P4/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t2p4_c1 <<- c(vetor_t2p4_c1, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t2p4_c1, fn_t2p4_c1)

vetor_t2p4_c1

t2p4_campanha1 <- tibble::tibble(`Índice de abetura` = vetor_t2p4_c1,
                                 Parcela = "T2P4",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 1")

t2p4_campanha1

## Campanha 2 ----

lista_t2p4_c2 <- list.files(path = "./imagens de dossel/T2P4",
                            pattern = "C2.jpg")

lista_t2p4_c2

vetor_t2p4_c2 <- c()

fn_t2p4_c2 <- function(x){

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T2P4/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T2P4/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t2p4_c2 <<- c(vetor_t2p4_c2, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t2p4_c2, fn_t2p4_c2)

vetor_t2p4_c2

t2p4_campanha2 <- tibble::tibble(`Índice de abetura` = vetor_t2p4_c2,
                                 Parcela = "T2P4",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 2")

t2p4_campanha2

## Campanha 3 ----

lista_t2p4_c3 <- list.files(path = "./imagens de dossel/T2P4",
                            pattern = "C3.jpg")

lista_t2p4_c3

vetor_t2p4_c3 <- c()

fn_t2p4_c3 <- function(x){

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T2P4/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T2P4/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t2p4_c3 <<- c(vetor_t2p4_c3, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t2p4_c3, fn_t2p4_c3)

vetor_t2p4_c3

t2p4_campanha3 <- tibble::tibble(`Índice de abetura` = vetor_t2p4_c3,
                                 Parcela = "T2P4",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 3")

t2p4_campanha3

## Unindo ----

df_t2p4 <- ls(pattern = "t2p4_campanha") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

df_t2p4

# T3P1 ----

## Campanha 1 ----

lista_t3p1_c1 <- list.files(path = "./imagens de dossel/T3P1",
                            pattern = "C1")

lista_t3p1_c1

vetor_t3p1_c1 <- c()

fn_t3p1_c1 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T3P1/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T3P1/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t3p1_c1 <<- c(vetor_t3p1_c1, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t3p1_c1, fn_t3p1_c1)

vetor_t3p1_c1

t3p1_campanha1 <- tibble::tibble(`Índice de abetura` = vetor_t3p1_c1,
                                 Parcela = "T3P1",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 1")

t3p1_campanha1

## Campanha 2 ----

lista_t3p1_c2 <- list.files(path = "./imagens de dossel/T3P1",
                            pattern = "C2")

lista_t3p1_c2

vetor_t3p1_c2 <- c()

fn_t3p1_c2 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T3P1/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T3P1/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t3p1_c2 <<- c(vetor_t3p1_c2, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t3p1_c2, fn_t3p1_c2)

vetor_t3p1_c2

t3p1_campanha2 <- tibble::tibble(`Índice de abetura` = vetor_t3p1_c2,
                                 Parcela = "T3P1",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 2")

t3p1_campanha2

## Campanha 3 ----

lista_t3p1_c3 <- list.files(path = "./imagens de dossel/T3P1",
                            pattern = "C3")

lista_t3p1_c3

vetor_t3p1_c3 <- c()

fn_t3p1_c3 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T3P1/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T3P1/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t3p1_c3 <<- c(vetor_t3p1_c3, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t3p1_c3, fn_t3p1_c3)

vetor_t3p1_c3

t3p1_campanha3 <- tibble::tibble(`Índice de abetura` = vetor_t3p1_c3,
                                 Parcela = "T3P1",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 3")

t3p1_campanha3

## Unindo ----

df_t3p1 <- ls(pattern = "t3p1_campanha") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

df_t3p1

# T3P2 ----

## Campanha 1 -----

lista_t3p2_c1 <- list.files(path = "./imagens de dossel/T3P2",
                            pattern = "C1")

lista_t3p2_c1

vetor_t3p2_c1 <- c()

fn_t3p2_c1 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T3P2/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T3P2/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t3p2_c1 <<- c(vetor_t3p2_c1, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t3p2_c1, fn_t3p2_c1)

vetor_t3p2_c1

t3p2_campanha1 <- tibble::tibble(`Índice de abetura` = vetor_t3p2_c1,
                                 Parcela = "T3P2",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 1")

t3p2_campanha1

## Campanha 2 ----

lista_t3p2_c2 <- list.files(path = "./imagens de dossel/T3P2",
                            pattern = "C2")

lista_t3p2_c2

vetor_t3p2_c2 <- c()

fn_t3p2_c2 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T3P2/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T3P2/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t3p2_c2 <<- c(vetor_t3p2_c2, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t3p2_c2, fn_t3p2_c2)

vetor_t3p2_c2

t3p2_campanha2 <- tibble::tibble(`Índice de abetura` = vetor_t3p2_c2,
                                 Parcela = "T3P2",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 2")

t3p2_campanha2

## Campanha 3 ----

lista_t3p2_c3 <- list.files(path = "./imagens de dossel/T3P2",
                            pattern = "C3")

lista_t3p2_c3

vetor_t3p2_c3 <- c()

fn_t3p2_c3 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/T3P2/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/T3P2/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_t3p2_c3 <<- c(vetor_t3p2_c3, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_t3p2_c3, fn_t3p2_c3)

vetor_t3p2_c3

t3p2_campanha3 <- tibble::tibble(`Índice de abetura` = vetor_t3p2_c3,
                                 Parcela = "T3P2",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 3")

t3p2_campanha3

## Unindo ----

df_t3p2 <- ls(pattern = "t3p2_campanha") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

df_t3p2

# R1 ----

## Campanha 1 ----

lista_r1_c1 <- list.files(path = "./imagens de dossel/R1",
                          pattern = "C1")

lista_r1_c1

vetor_r1_c1 <- c()

fn_r1_c1 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/R1/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/R1/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_r1_c1 <<- c(vetor_r1_c1, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_r1_c1, fn_r1_c1)

vetor_r1_c1

r1_campanha1 <- tibble::tibble(`Índice de abetura` = vetor_r1_c1,
                               Parcela = "R1",
                               Ponto = paste0("Ponto ", seq(0, 250, 50)),
                               Campanha = "Campanha 1")

r1_campanha1

## Campanha 2 ----

lista_r1_c2 <- list.files(path = "./imagens de dossel/R1",
                          pattern = "C2")

lista_r1_c2

vetor_r1_c2 <- c()

fn_r1_c2 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/R1/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/R1/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_r1_c2 <<- c(vetor_r1_c2, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_r1_c2, fn_r1_c2)

vetor_r1_c2

r1_campanha2 <- tibble::tibble(`Índice de abetura` = vetor_r1_c2,
                               Parcela = "R1",
                               Ponto = paste0("Ponto ", seq(0, 250, 50)),
                               Campanha = "Campanha 2")

r1_campanha2

## Campanha 3 ----

lista_r1_c3 <- list.files(path = "./imagens de dossel/R1",
                          pattern = "C3")

lista_r1_c3

vetor_r1_c3 <- c()

fn_r1_c3 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/R1/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/R1/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_r1_c3 <<- c(vetor_r1_c3, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_r1_c3, fn_r1_c3)

vetor_r1_c3

r1_campanha3 <- tibble::tibble(`Índice de abetura` = vetor_r1_c3,
                               Parcela = "R1",
                               Ponto = paste0("Ponto ", seq(0, 250, 50)),
                               Campanha = "Campanha 3")

r1_campanha3

## Unindo ----

df_r1 <- ls(pattern = "r1_campanha") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

df_r1

# R2 ----

## Campanha 1 ----

lista_r2_c1 <- list.files(path = "./imagens de dossel/R2",
                            pattern = "C1")

lista_r2_c1

vetor_r2_c1 <- c()

fn_r2_c1 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/R2/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/R2/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_r2_c1 <<- c(vetor_r2_c1, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_r2_c1, fn_r2_c1)

vetor_r2_c1

r2_campanha1 <- tibble::tibble(`Índice de abetura` = vetor_r2_c1,
                                 Parcela = "R2",
                                 Ponto = paste0("Ponto ", seq(0, 250, 50)),
                                 Campanha = "Campanha 1")

r2_campanha1

## Campanha 2 ----

lista_r2_c2 <- list.files(path = "./imagens de dossel/R2",
                          pattern = "C2")

lista_r2_c2

vetor_r2_c2 <- c()

fn_r2_c2 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/R2/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/R2/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_r2_c2 <<- c(vetor_r2_c2, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_r2_c2, fn_r2_c2)

vetor_r2_c2

r2_campanha2 <- tibble::tibble(`Índice de abetura` = vetor_r2_c2,
                               Parcela = "R2",
                               Ponto = paste0("Ponto ", seq(0, 250, 50)),
                               Campanha = "Campanha 2")

r2_campanha2

## Campanha 3 ----

lista_r2_c3 <- list.files(path = "./imagens de dossel/R2",
                          pattern = "C3")

lista_r2_c3

vetor_r2_c3 <- c()

fn_r2_c3 <- function(x){

  paste0("Iniciando as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

  raster <- terra::rast(paste0("./imagens de dossel/R2/", x))

  paste0("plotando: ", x |> stringr::str_remove(".jpg")) |>
    crayon::yellow() |>
    message()

  ggplt <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = raster) +
    scale_fill_continuous(na.value = "transparent") +
    labs(title = x) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(ggplt)

  paste0("binarizando: ", x) |>
    crayon::yellow() |>
    message()

  x2 <- paste0("./imagens de dossel/R2/", x)

  file <- x2 |>
    hemispheR::import_fisheye()  |>
    hemispheR::binarize_fisheye()

  ggplt_bn <- ggplot() +
    tidyterra::geom_spatraster(data = file) +
    scale_fill_viridis_c(na.value = "transparent", breaks = seq(0, 1, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = x) +
    theme_bw()

  print(ggplt_bn)

  paste0("analizando: ", x) |>
    crayon::yellow() |>
    message()

  file_abertura <- file[file > 0] |>
    terra::ncell()

  file_total <- file |>
    terra::ncell()

  porcentagem_abertura_docel <- file_abertura / file_total

  print(porcentagem_abertura_docel)

  vetor_r2_c3 <<- c(vetor_r2_c3, porcentagem_abertura_docel)

  paste0("Encerrado as análises para ", x |> stringr::str_remove(".jpg")) |>
    crayon::green() |>
    message()

}

purrr::walk(lista_r2_c3, fn_r2_c3)

vetor_r2_c3

r2_campanha3 <- tibble::tibble(`Índice de abetura` = vetor_r2_c3,
                               Parcela = "R2",
                               Ponto = paste0("Ponto ", seq(0, 250, 50)),
                               Campanha = "Campanha 3")

r2_campanha3

## Unindo ----

df_r2 <- ls(pattern = "r2_campanha") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

df_r2

# Unido ----

## Criando o dataframe ----

dossel_df <- ls(pattern = "df_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::arrange(Campanha)

dossel_df |> as.data.frame()

## Exportando ----

dossel_df |>
  writexl::write_xlsx("abertura_dossel.xlsx")
