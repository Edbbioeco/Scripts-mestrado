# Pacotes ----

library(sf)

library(tidyverse)

library(leaflet)

library(leaflet.extras)

library(leafem)

# Shapefile da borda ----

## Importar ----

borda <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/borda_saltinho.shp")

## Visualizar ----

borda

ggplot() +
  geom_sf(data = borda, color = "black")
