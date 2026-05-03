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
