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
