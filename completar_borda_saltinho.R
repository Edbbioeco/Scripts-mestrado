# Pacotes ----

library(sf)

library(tidyverse)

library(leaflet)

library(leaflet.extras)

library(leafem)

# Dados ----

## Shapefile da borda ----

### Importar ----

borda <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/borda_saltinho.shp")

### Visualizar ----

borda

ggplot() +
  geom_sf(data = borda, color = "black")

## Raster de Saltinho ----

### Importar ----

saltinho <- terra::rast("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/saltinhocdse_rgb.tif")

### Visualizar ----

saltinho

ggplot() +
  tidyterra::geom_spatraster_rgb(data = saltinho) +
  geom_sf(data = borda, color = "red", fill = "transparent")
