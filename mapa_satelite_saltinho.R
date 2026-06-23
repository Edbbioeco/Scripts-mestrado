# PAcotes ----

library(terra)

library(tidyverse)

library(tidyterra)

library(ggspatial)

library(ggview)

# Raster de satélite ----

## Importar ----

saltinho <- terra::rast("saltinho.tif")

## Visualizar ----

saltinho

ggplot() +
  tidyterra::geom_spatraster_rgb(data = saltinho) +
  coord_sf(expand = FALSE)
