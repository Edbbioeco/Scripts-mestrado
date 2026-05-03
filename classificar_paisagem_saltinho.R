# Pacotes ----

library(sf)

library(tidyverse)

library(leaflet)

library(leaflet.extras)

library(leafem)

library(maptiles)

library(tidyterra)

library(terra)

library(randomForest)

library(ggspatial)

library(ggview)

# Dados ----

## Shapéfile de Saltinho ----

### Saltinhop ----

saltinho <- sf::st_read("Saltinho.shp")

### Visualizar ----

saltinho

ggplot() +
  geom_sf(data = saltinho)

## Imagem de satélite ----

### Baixar ----

saltinho_sat <- terra::rast("saltinho.tif")

### Visualizar ----

saltinho_sat

ggplot() +
  tidyterra::geom_spatraster_rgb(data = saltinho_sat) +
  geom_sf(data = saltinho, color = "red", fill = "transparent", size = 1) +
  coord_sf(expand = FALSE)
