# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

library(terra)

library(tidyterra)

library(ggspatial)

library(ggview)

library(patchwork)

# Dados ----

## Brasil ----

### Importando ----

br <- geobr::read_state(year = 2019)

### Visualizando ----

ggplot() +
  geom_sf(data = br, color = "black")

## Pernambuco ----

### Importando ----

pe <- br |>
  dplyr::filter(abbrev_state == "PE")

### Visualizando ----

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = pe, color = "black", fill = "gold")

## Saltinho ----

### Importando ----

saltinho <- sf::st_read("saltinho.shp")

### Visualizando ----

ggplot() +
  geom_sf(data = pe, color = "black", fill = "gold") +
  geom_sf(data = saltinho, color = "black", fill = "forestgreen")
