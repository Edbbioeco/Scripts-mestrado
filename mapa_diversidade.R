# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(terra)

library(tidyterra)

library(vegan)

library(ggview)

# Dados ----

## Matriz de composição ----

### Importar ----

comp <- readxl::read_xlsx("matriz_composicao.xlsx")

### Visualizar ----

comp

comp |> dplyr::glimpse()

## Shapefile da borda de Saltinho ----

### Importar ----

borda <- sf::st_read("borda_saltinho.shp")

### Visualizar ----

borda

ggplot() +
  geom_sf(data = borda, color = "black", linewidth = 1)

## Coordenadas das parcelas ----

### Importar ----

parcelas <- sf::st_read("coordenadas_parcelas_saltinho.shp")

### Visualizar ----

parcelas

ggplot() +
  geom_sf(data = borda, color = "black", linewidth = 1) +
  geom_sf(data = parcelas, color = "black", linewidth = 1)
