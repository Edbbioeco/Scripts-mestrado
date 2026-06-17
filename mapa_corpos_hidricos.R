# Pacotes ----

library(sf)

library(tidyverse)

library(geobr)

library(terra)

library(tidyterra)

library(ggspatial)

library(ggview)

# Dados ----

## Parcelas ----

### Importnado ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizando ----

parcelas

ggplot() +
  geom_sf(data = parcelas, color = "black", linewidth = 1)

### Trtando ----

parcelas_trat <- parcelas |>
  dplyr::mutate(tipo = c(rep("Uniform Sample", 10),
                         rep("Riparian Sample", 2)))

parcelas_trat

ggplot() +
  geom_sf(data = parcelas_trat, aes(color = tipo), linewidth = 1)

## borda ----

### Importando ----

borda <- sf::st_read("borda_saltinho.shp")

### Visualizando ----

borda

ggplot() +
  geom_sf(data = borda, color = "black", linewidth = 1)

## Corpos hídricos ----

### Importando ----

corpos_hid <- sf::st_read("corpos_hidricos_saltinho.gpkg")

### Visualizando ----

corpos_hid

# Shaepfile de distância das parcelas para os corpos hídricos ----

corpos_hid_ref <- c(corpos_hid |>
                      dplyr::filter(geom |>
                                      sf::st_geometry_type() |>
                                      stringr::str_detect("POLYGON")) |>
                      sf::st_geometry() |>
                      sf::st_boundary(),
                    corpos_hid |>
                      dplyr::filter(geom |>
                                      sf::st_geometry_type() |>
                                      stringr::str_detect("LINESTRING")) |>
                      sf::st_geometry()) |>
  sf::st_union()

corpos_hid_ref

dist_hid <- parcelas |>
  sf::st_nearest_points(corpos_hid_ref) |>
  sf::st_as_sf() |>
  dplyr::mutate(Trlh.Pr = parcelas$Trlh.Pr)

dist_hid

ggplot() +
  geom_sf(data = borda) +
  geom_sf(data = corpos_hid, color = "blue") +
  geom_sf(data = dist_hid, color = "red") +
  geom_sf(data = parcelas, color = "black")

# Mapa ----

ggplot() +
  geom_sf(data = borda, color = "green4", linewidth = 1, fill = "transparent") +
  geom_sf(data = corpos_hid, color = "blue", linewidth = 1) +
  geom_sf(data = dist_hid |>
            dplyr::filter(Trlh.Pr != "1-1"),
          color = "red", linewidth = 1) +
  geom_sf(data = parcelas |>
            dplyr::filter(Trlh.Pr != "1-1"),
          color = "black", linewidth = 1) +
  coord_sf(label_graticule = "NSWE") +
  ggspatial::annotation_scale(location = "tr",
                              text_face = "bold",
                              text_cex = 2,
                              text_col = "black",
                              unit_category = "metric",
                              bar_cols = c("black", "gold")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        panel.border = element_rect(color = "black", size = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_corpos_hidricos_seminarios2.png", height = 10, width = 12)
