# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

# Dados ----

## Shapefile de SAltinho ----

### Importando ----

saltinho <- sf::st_read("Saltinho.shp")

### mVisualizando ----

saltinho

ggplot() +
  geom_sf(data = saltinho, color = "black")

## Parcelas ----

### Importnado ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizando ----

parcelas

ggplot() +
  geom_sf(data = parcelas, color = "black", linewidth = 1)

## Rasters ----

### Importando ----

list.files(path = "./rasters")

importando_rasters <- function(nome, caminho){

  paste0("Importando ", nome) |>
    crayon::yellow() |>
    message()

  raster <- terra::rast(caminho)

  assign(paste0("raster_", nome),
         raster,
         envir = globalenv())

  paste0("importação de ", nome, " concluída") |>
    crayon::green() |>
    message()

  message()

}

nome <- c("EVI", "NDVI", "angulo_zenite_solar", "qualidade_vegetação")

nome

caminho <- paste0("./rasters/",
                  list.files(path = "./rasters"))

caminho

purrr::walk2(nome,
             caminho,
             importando_rasters)

### Visualizando ----

rasters <- ls(pattern = "raster_") |>
  mget(envir = globalenv())

names(rasters) <- nome

rasters

visualizar_rasters <- function(rasters_gg){

  gg_raster <- ggplot() +
    tidyterra::geom_spatraster(data = rasters_gg) +
    geom_sf(data = saltinho,
            color = "red", fill = "transparent",
            linewidth = 1) +
    geom_sf(data = parcelas,
            color = "red", fill = "transparent",
            linewidth = 1) +
    facet_wrap(~lyr) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_viridis_c(na.value = "transparent",
                         option = "turbo")

  print(gg_raster)

}

purrr::walk(rasters,
            visualizar_rasters)

# Extraindo os dados -----

## Calculando os centroides ----

centroides <- parcelas |>
  dplyr::filter(Trlh.Pr != "1-1") |>
  sf::st_centroid()

centroides

## Extraindo os valores ----

rasters_unidos <- ls(pattern = "raster_") |>
  mget(envir = globalenv()) |>
  terra::rast()

names(rasters_unidos) <- nome

rasters_unidos

### Criadno um raster unico ----

valores <- rasters_unidos |>
  terra::extract(centroides)

valores

## Unindo os dataframes ----

source("modelos_lineares.R")

df_unido <- dplyr::bind_cols(div_alfa, valores) |>
  dplyr::select(-ID)

df_unido

# Modelos lineares ----

## Modelo Q0 ----

### Criando o modelo ----

modelo_q0 <- glm(`0` ~ EVI +
                   NDVI +
                   angulo_zenite_solar +
                   qualidade_vegetação,
                 data = df_unido,
                 family = poisson(link = "log"))

### Avaliando o modelo ----

par(mfrow = c(2, 2)); modelo_q0 |>plot(); par(mfrow = c(1, 1))

### Estatísticas do modelo ----

modelo_q0 |> summary()

## Modelo Q1 ----

### Criando o modelo ----

modelo_q1 <- lm(`1` ~ EVI +
                  NDVI +
                  angulo_zenite_solar +
                  qualidade_vegetação,
                data = df_unido)

### Avaliando o modelo ----

modelo_q1 |> performance::check_model(check = c("vif",
                                                "qq",
                                                "normality",
                                                "homogeneity"))

### Estatísticas do modelo ----

modelo_q1 |> summary()

# Gráficos ----

## Q0 ----

df_unido |>
  tidyr:::pivot_longer(cols = EVI:qualidade_vegetação,
                       names_to = "indices",
                       values_to = "valores") |>
  ggplot(aes(valores, `0`, fill = indices)) +
  geom_point(shape = 21, color = "black", size = 3) +
  facet_wrap(~indices,
             scales = "free_x")

## Q1 ----

df_unido |>
  tidyr:::pivot_longer(cols = EVI:qualidade_vegetação,
                       names_to = "indices",
                       values_to = "valores") |>
  ggplot(aes(valores, `1`, fill = indices)) +
  geom_point(shape = 21, color = "black", size = 3) +
  facet_wrap(~indices,
             scales = "free_x")
