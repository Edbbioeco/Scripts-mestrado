# Pacotes ----

library(sf)

library(tidyverse)

library(maptiles)

library(tidyterra)

library(terra)

library(leaflet)

library(leaflet.extras)

library(leafem)

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

saltinho_sat <- saltinho |>
  maptiles::get_tiles(provider = "Esri.WorldImagery",
                      zoom = 18)

### Visualizar ----

saltinho_sat

ggplot() +
  tidyterra::geom_spatraster_rgb(data = saltinho_sat) +
  geom_sf(data = saltinho, color = "red", fill = "transparent", size = 1) +
  coord_sf(expand = FALSE)

### Exportar ----

saltinho_sat |>
  terra::writeRaster("saltinho_sat.tif")

# Pontos de treino ----

## Criar o mapa de visualização ----

mapa <- leaflet::leaflet() |>
  leaflet::addProviderTiles(provider = providers$Esri.WorldImagery) |>
  leaflet.extras::addDrawToolbar(targetGroup = "Draw",
                                 circleOptions = TRUE,
                                 polygonOptions = TRUE,
                                 rectangleOptions = TRUE,
                                 markerOptions = TRUE,
                                 polylineOptions = TRUE,
                                 circleMarkerOptions = TRUE,
                                 editOptions = leaflet.extras::editToolbarOptions()) |>
  leafem::addMouseCoordinates() |>
  leaflet::addPolygons(data = saltinho,
                       color = "gold",
                       fillOpacity = 0)

mapa
