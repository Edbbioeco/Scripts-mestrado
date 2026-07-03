# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(writexl)

# Dados ----

## Parcelas ----

### Importar ----

parcelas <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/saltinho_ppbio_parcelas.shp")

### Visualizar ----

parcelas

ggplot() +
  geom_sf(data = parcelas)

## Borda ----

### Importar ----

borda <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/borda_saltinho_recortado.shp")

### Visualizar ----

borda

ggplot() +
  geom_sf(data = borda, color = "black") +
  geom_sf(data = parcelas, color = "red")

## Altitude de Saltinho ----

### Importar ----

alt <- terra::rast("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/altitude.tif")

### Visualizar ----

alt

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  geom_sf(data = borda, color = "red", fill = "transparent") +
  geom_sf(data = parcelas, color = "red") +
  scale_fill_viridis_c() +
  coord_sf(expand = FALSE)

# Distância da borda ----

## Distância geodésica simples da borda ----

distancia_geodesica_borda <- parcelas |>
  sf::st_centroid() |>
  sf::st_distance(borda[2, ] |>
                    sf::st_boundary()) |>
  as.numeric()

distancia_geodesica_borda

## Ponto mais próximo da borda ----

ponto_geodesica_borda <- parcelas |>
  sf::st_centroid() |>
  sf::st_nearest_points(borda[2, ] |>
                          sf::st_boundary()) |>
  sf::st_as_sf(crs = 4674)

ponto_geodesica_borda

ggplot() +
  geom_sf(data = borda, color = "black") +
  geom_sf(data = parcelas, color = "red") +
  geom_sf(data = ponto_geodesica_borda, color = "blue")

## Valores de altitude ----

### Centróides das parcelas ----

valores_altitude_centroides <- alt |>
  terra::extract(parcelas |>
                   sf::st_centroid()) |>
  dplyr::pull(2)

valores_altitude_centroides

### Ponto da borda mais próxima ----

valores_altitude_borda <- alt |>
  terra::extract(ponto_geodesica_borda |>
                   sf::st_coordinates() |>
                   as.data.frame() |>
                   dplyr::mutate(Parcela = parcelas$Trlh.Pr |>
                                   rep(each = 2)) |>
                   dplyr::group_by(Parcela) |>
                   dplyr::slice(2) |>
                   sf::st_as_sf(coords = c(1:2),
                                crs = 4674)) |>
  dplyr::pull(2)

valores_altitude_borda

## Distância compensando a altitude ----

dist_comp <- sqrt(distancia_geodesica_borda^2 +
                    (valores_altitude_centroides - valores_altitude_borda)^2)

dist_comp

## Criar o data frame dos valores ----

df_dist_borda <- tibble::tibble(Parcela = parcelas$Trlh.Pr,
                                `Distância da borda` = dist_comp)

df_dist_borda

df_dist_borda |> dplyr::glimpse()
