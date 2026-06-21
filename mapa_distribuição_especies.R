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

## Shapefile das parcelas ----

### Importar ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizar ----

parcelas

ggplot() +
  geom_sf(data = borda, color = "black") +
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
