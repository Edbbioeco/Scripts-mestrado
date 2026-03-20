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

## Borda da mata ----

### Importando ----

borda <- sf::st_read("borda_saltinho.shp")

### Visualizando ----

ggplot() +
  geom_sf(data = saltinho, color = "black", fill = "transparent", linewidth = 1) +
  geom_sf(data = borda, color = "darkgreen", fill = "transparent", linewidth = 1)

## Parcelas de amostragem ----

### Importando ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizando ----

ggplot() +
  geom_sf(data = saltinho, color = "black", fill = "transparent", linewidth = 1) +
  geom_sf(data = borda, color = "darkgreen", fill = "transparent", linewidth = 1) +
  geom_sf(data = parcelas, color = "red", linewidth = 1)

## Imagem de satélite ----

### Importando ----

saltinho_tif <- terra::rast("saltinho.tif")

### Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = saltinho_tif) +
  geom_sf(data = saltinho, color = "yellow", fill = "transparent", linewidth = 1) +
  geom_sf(data = borda, color = "green4", fill = "transparent", linewidth = 1) +
  geom_sf(data = parcelas, color = "red", linewidth = 1) +
  coord_sf(expand = FALSE)

# Mapa do Brasil ----

br_map <- ggplot() +
  geom_sf(data = br, color = "black", fill = "lightgray", linewidth = 1) +
  geom_sf(data = pe, color = "black", fill = "gold", linewidth = 1) +
  ggspatial::coord_sf(expand = FALSE,
                      label_graticule = "SE") +
  theme(axis.text = element_text(size = 20)) +
  ggview::canvas(height = 10, width = 12)

br_map
