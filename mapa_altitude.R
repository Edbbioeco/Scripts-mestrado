# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(parzer)

library(elevatr)

library(tidyterra)

library(ggspatial)

library(ggview)

# Dados ----

## Dados de abundância de espécies ----

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

## Shapefile das parcelas -----

### Importnado ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizando ----

parcelas

ggplot() +
  geom_sf(data = borda, color = "red", linewidth = 1) +
  geom_sf(data = saltinho, color = "black", linewidth = 1, fill = "transparent") +
  geom_sf(data = parcelas, color = "black", linewidth = 1)

## Topografia -----

### Shapefile de base para a área ----

shp_area <- tibble::tibble(x = c("35°12'18.33\"W",
                                 "35°12'18.33\"W",
                                 "35°09'16.68\"W",
                                 "35°09'16.68\"W") |>
                             parzer::parse_lon(),
                           y = c("08°42'37.43\"S",
                                 "08°44'42.43\"S",
                                 "08°44'42.43\"S",
                                 "08°42'37.43\"S") |>
                             parzer::parse_lat()) |>
  sf::st_as_sf(coords = c("x", "y"),
               crs = 4674) |>
  dplyr::summarise(geometry = geometry |>
                     sf::st_combine()) |>
  st_cast("POLYGON")

shp_area

ggplot() +
  geom_sf(data = shp_area, color = "black", fill = "green4", linewidth = 1) +
  geom_sf(data = borda, color = "red", linewidth = 1) +
  geom_sf(data = saltinho, color = "black", linewidth = 1, fill = "transparent") +
  geom_sf(data = parcelas, color = "black", linewidth = 1)

### Importandoo ----

alt <- elevatr::get_aws_terrain(locations = shp_area,
                                z = 14,
                                prj = 4674,
                                clip = "locations") |>
  terra::mask(shp_area) |>
  terra::crop(shp_area)

### Visualizando ----

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  geom_sf(data = borda, color = "red", linewidth = 1, fill = "transparent") +
  geom_sf(data = saltinho, color = "black", linewidth = 1, fill = "transparent") +
  geom_sf(data = parcelas, color = "black", linewidth = 1) +
  tidyterra::scale_fill_terrain_c()

# Mapa -----

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  geom_sf(data = borda, color = "darkgreen", linewidth = 1, fill = "transparent") +
  geom_sf(data = saltinho, color = "black", linewidth = 1, fill = "transparent") +
  geom_sf(data = parcelas, color = "black", linewidth = 1) +
  tidyterra::scale_fill_whitebox_c(palette = "arid",
                                   direction = -1,
                                   name = "Altitude",
                                   guide = guide_colorbar(title.position = "top",
                                                           title.hjust = 0.5,
                                                           barwidth = 20,
                                                           frame.colour = "black",
                                                           frame.linewidth = 1,
                                                           ticks.colour = "black",
                                                           ticks.linewidth = 1)) +
  coord_sf(expand = FALSE, label_graticule = "NSWE") +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_altitude_seminarios2.png", height = 10, width = 12)
