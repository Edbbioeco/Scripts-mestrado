# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(vegan)

library(ggview)

library(patchwork)

# Dados ----

## Dados de composição ----

### Importando ----

especies <- readxl::read_xlsx("levantamento_anuros.xlsx")

## Visualizando ----

especies

especies %>% dplyr::glimpse()

## Shapefile de Saltinho ----

### Importando ----

saltinho <- sf::st_read("Saltinho.shp")

### Visualizando ----

saltinho

saltinho |>
  ggplot() +
  geom_sf(data = saltinho, color = "black", linewidth = 1)

## Shapefile da borda da mata ----

### Importando ----

borda <- sf::st_read("borda_saltinho.shp")

### Visualizando ----

borda

ggplot() +
  geom_sf(data = borda, color = "red", linewidth = 1) +
  geom_sf(data = saltinho, color = "black", linewidth = 1, fill = "transparent")

## Shapefile dos corpos hídricos -----

### Importando ----

hidrico <- sf::st_read("malha_hidrica/malha_hidrica.shp")

### Recortando ----

hidrico_rec <- hidrico |>
  sf::st_set_crs(4674) |>
  sf::st_intersection(saltinho)

hidrico_rec

### Visualizando ----

ggplot() +
  geom_sf(data = borda, color = "red", linewidth = 1) +
  geom_sf(data = saltinho, color = "black", linewidth = 1, fill = "transparent") +
  geom_sf(data = hidrico_rec, color = "blue", linewidth = 1)

# Diversidade ----

# Mapa de hidrografia ----
