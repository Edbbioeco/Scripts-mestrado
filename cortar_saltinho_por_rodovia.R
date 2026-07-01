# Pacotes ----

library(sf)

library(tidyverse)

library(leaflet)

library(leaflet.extras)

library(leafem)

library(mapedit)

# Dados ----

## Borda ----

### Importar ----

borda <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/borda_saltinho.shp")

### Visualizar ----

borda

ggplot() +
  geom_sf(data = borda, color = "black")

## Rodovias ----

### Criar mapa interativo ----

mapa <- leaflet::leaflet() |>
  leaflet::addProviderTiles(providers$Esri.WorldImagery) |>
  leaflet.extras::addDrawToolbar(
    editOptions = leaflet.extras::editToolbarOptions()) |>
  leafem::addMouseCoordinates() |>
  leaflet::addPolygons(data = borda,
                       color = "red",
                       fillOpacity = 0)

mapa
