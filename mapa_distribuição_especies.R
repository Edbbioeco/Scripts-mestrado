# PAcotes ----

library(readxl)

library(tidyverse)

library(sf)

library(terra)

library(terra)

library(ggspatial)

library(ggview)

# Dados ----

## Dados de composição ----

### Importar ----

comp <- readxl::read_xlsx("matriz_composicao.xlsx")

### Visualizar ----

comp

comp |> dplyr::glimpse()

## Shapefile da borda da Mata de Saltinho ----

### Importar ----

borda <- sf::st_read("borda_saltinho.shp")

### Visualizar ----

borda

ggplot() +
  geom_sf(data = borda, color = "black")

## Shapefile dos corpos hídricos ----

### Importar ----

hid <- sf::st_read("corpos_hidricos_saltinho.gpkg")

### Visualizar ----

hid

ggplot() +
  geom_sf(data = borda, color = "black") +
  geom_sf(data = hid, color = "blue")

## Shapefile das parcelas ----

### Importar ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizar ----

parcelas

ggplot() +
  geom_sf(data = borda, color = "black") +
  geom_sf(data = hid, color = "blue") +
  geom_sf(data = parcelas, color = "black")

## Raster de altitude ----

### Importar ----

alt <- terra::rast("altitude.tif")

### Importar ----

alt

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  geom_sf(data = borda, color = "gold", fill = "transparent") +
  geom_sf(data = parcelas, color = "gold", fill = "transparent")

# Mapa ----

## AbundÂncia por parcela ----

parcela_abund <- parcelas[-1, ] |>
  sf::st_centroid() |>
  dplyr::bind_cols(comp[, -1]) |>
  tidyr::pivot_longer(cols = 2:11,
                      names_to = "Espécie",
                      values_to = "Abundância")

parcela_abund
