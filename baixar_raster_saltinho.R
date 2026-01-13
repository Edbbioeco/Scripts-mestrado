# Pacotes ----

library(sf)

library(tidyverse)

library(maptiles)

library(tidyterra)

library(terra)

# Dados ----

## Importando ----

saltinho <- sf::st_read("Saltinho.shp")

## Visualizando ----

saltinho |>
  ggplot() +
  geom_sf(color = "black", linewidth = 1)

# Imgem de satélite ----

## Importando ----

saltinho_tif <- maptiles::get_tiles(x = saltinho,
                                    provider = "Esri.WorldImagery",
                                    zoom = 15,
                                    retina = TRUE)

## Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = saltinho_tif) +
  geom_sf(data = saltinho, color = "yellow", linewidth = 1, fill = "transparent")
