# Pacotes ----

library(sf)

library(tidyverse)

library(ggview)

# Dados ----

## Parcelas ----

### Importar ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizar ----

parcelas

ggplot() +
  geom_sf(data = parcelas)

## Borda de Saltinho ----

### Importar ----

borda <- sf::st_read("borda_saltinho.shp")

### Visualizar ----

borda

ggplot() +
  geom_sf(data = borda) +
  geom_sf(data = parcelas)

# Distância da borda ----

## Shapefile da distância mínima de cada parcela ----

dist_borda <- parcelas |>
  dplyr::filter(Trlh.Pr != "1-1") |>
  sf::st_centroid() |>
  sf::st_nearest_points(borda |>
                          sf::st_boundary())

dist_borda

ggplot() +
  geom_sf(data = borda) +
  geom_sf(data = dist_borda, color = "red") +
  geom_sf(data = parcelas, color = "blue")
