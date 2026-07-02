# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(leaflet)

library(leaflet.extras)

library(leafem)

library(mapedit)

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

# Completar o shapefile ----

## Criar mapa editável ----

mapa_editavel <- leaflet::leaflet() |>
  leafem::addRasterRGB(saltinho) |>
  leaflet.extras::addDrawToolbar(
    targetGroup = "Draw",
    polylineOptions = leaflet.extras::drawPolylineOptions(),
    polygonOptions = leaflet.extras::drawPolygonOptions(),
    circleOptions = leaflet.extras::drawCircleOptions(),
    rectangleOptions = leaflet.extras::drawRectangleOptions(),
    markerOptions = leaflet.extras::drawMarkerOptions(),
    circleMarkerOptions = leaflet.extras::drawCircleMarkerOptions(),
    editOptions = leaflet.extras::editToolbarOptions()) |>
  leafem::addMouseCoordinates() |>
  leaflet::addPolygons(data = borda |>
                         sf::st_transform(crs = 4326),
                       color = "red",
                       fillOpacity = 0)

mapa_editavel

## Criar o shapefile de complemento ----

shp_comp <- mapa_editavel |> mapedit::editMap()

shp <- shp_comp$drawn |>
  sf::st_transform(crs = borda |> sf::st_crs())

shp

ggplot() +
  tidyterra::geom_spatraster_rgb(data = saltinho) +
  geom_sf(data = borda, color = "red", fill = "transparent") +
  geom_sf(data = shp, color = "gold", fill = "transparent")
