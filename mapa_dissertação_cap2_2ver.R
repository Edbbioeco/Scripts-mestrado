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

br

ggplot() +
  geom_sf(data = br, color = "black")

## Pernambuco ----

### Importando ----

pe <- br |>
  dplyr::filter(abbrev_state == "PE")

### Visualizando ----

pe

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = pe, color = "black", fill = "gold")

## Saltinho ----

### Importando ----

saltinho <- sf::st_read("saltinho.shp")

### Visualizando ----

saltinho

ggplot() +
  geom_sf(data = pe, color = "black", fill = "gold") +
  geom_sf(data = saltinho, color = "black", fill = "forestgreen")

## Borda da mata ----

### Importando ----

borda <- sf::st_read("borda_saltinho.shp")

### Visualizando ----

borda

ggplot() +
  geom_sf(data = saltinho, color = "black", fill = "transparent", linewidth = 1) +
  geom_sf(data = borda, color = "darkgreen", fill = "transparent", linewidth = 1)

## Parcelas de amostragem ----

### Importando ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizando ----

parcelas

ggplot() +
  geom_sf(data = saltinho, color = "black", fill = "transparent", linewidth = 1) +
  geom_sf(data = borda, color = "darkgreen", fill = "transparent", linewidth = 1) +
  geom_sf(data = parcelas, color = "red", linewidth = 1)

### Tratando ----

parcelas_trat <- parcelas |>
  dplyr::mutate(tipo = c(rep("Uniform Sample", 10),
                         rep("Riparian Sample", 2))) |>
  dplyr::filter(!Trlh.Pr == "1-1") |>
  sf::st_centroid()

parcelas_trat

ggplot() +
  geom_sf(data = saltinho, color = "black", fill = "transparent", linewidth = 1) +
  geom_sf(data = borda, color = "darkgreen", fill = "transparent", linewidth = 1) +
  geom_sf(data = parcelas_trat, aes(fill = tipo), color = "black",
          size = 5, shape = 21, stroke = 1)

## Corpos hídricos ----

### Importando ----

corpos_hid <- sf::st_read("corpos_hidricos_saltinho.gpkg")

### Visualizando ----

corpos_hid

ggplot() +
  geom_sf(data = saltinho, color = "black", fill = "transparent", linewidth = 1) +
  geom_sf(data = borda, color = "darkgreen", fill = "transparent", linewidth = 1) +
  geom_sf(data = parcelas, color = "red", linewidth = 1) +
  geom_sf(data = corpos_hid, color = "royalblue", fill = "transparent", linewidth = 1)

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

# Setando tema ----

source("C:/Users/LENOVO/OneDrive/Documentos/funções/tema.R")

# Mapa do Brasil ----

br_map <- ggplot() +
  geom_sf(data = br, color = "black", fill = "lightgray") +
  geom_sf(data = pe, color = "black", fill = "lightgoldenrod") +
  ggspatial::coord_sf(expand = FALSE,
                      label_graticule = "NW") +
  theme(axis.text = element_text(size = 20)) +
  ggview::canvas(height = 10, width = 12)

br_map

# Mapa de Pernambuco ----

pe_map <- ggplot() +
  geom_sf(data = br, color = "black", fill = "lightgray", linewidth = 0.5) +
  geom_sf(data = pe, color = "black", fill = "lightgoldenrod", linewidth = 0.5) +
  geom_sf(data = saltinho, color = "red", fill = "transparent", linewidth = 1) +
  ggspatial::coord_sf(label_graticule = "NE",
                      xlim = c(-36.3, -34.8),
                      ylim = c(-8.9, -7.4)) +
  scale_x_continuous(breaks = seq(-36.2, -34.8, 0.4)) +
  theme(axis.text = element_text(size = 20)) +
  ggview::canvas(height = 10, width = 12)

pe_map

# Mapa de Saltinho ----

mapa_principal <- ggplot() +
  geom_sf(data = br, color = "black",
          aes(fill = "Brazil"), linewidth = 0.5) +
  geom_sf(data = pe, color = "black",
          aes(fill = "Pernambuco"), linewidth = 0.75) +
  tidyterra::geom_spatraster_rgb(data = saltinho_tif) +
  geom_sf(data = borda,
          aes(color = "Native Forest"),
          linewidth = 1, fill = "transparent") +
  geom_sf(data = corpos_hid,
          aes(color = "Hidric bodies"),
          linewidth = 1, fill = "transparent") +
  geom_sf(data = saltinho,
          aes(color = "REBio Saltinho"),
          linewidth = 1, fill = "transparent") +
  scale_fill_manual(values = c("Brazil" = "lightgray",
                               "Pernambuco" = "lightgoldenrod"),
                    breaks = c("Brazil", "Pernambuco")) +
  scale_color_manual(values = c("Native Forest" = "gold3",
                                "REBio Saltinho" = "red",
                                "Hidric bodies" = "royalblue",
                                "Uniform Sample" = "black",
                                "Riparian Sample" = "black"),
                     breaks = c("Native Forest", "Hidric bodies", "REBio Saltinho",
                                "Uniform Sample", "Riparian Sample")) +
  guides(fill = guide_legend(order = 1, nrow = 2, title = NULL),
         color = guide_legend(order = 2, nrow = 2)) +
  labs(fill = NULL,
       color = NULL) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = parcelas_trat, aes(fill = tipo),
          color = "black", shape = 21, size = 5, stroke = 1) +
  scale_fill_manual(values = c("Uniform Sample" = "orange2",
                               "Riparian Sample" = "purple"),
                    breaks = c("Uniform Sample", "Riparian Sample")) +
  guides(fill = guide_legend(order = 3, nrow = 2)) +
  labs(fill = NULL) +
  ggspatial::coord_sf(label_graticule = "SEW",
                      xlim = c(-35.20319, -35.15696),
                      ylim = c(-8.7442, -8.710025),
                      expand = FALSE) +
  ggspatial::annotation_scale(location = "br",
                              text_face = "bold",
                              text_cex = 2,
                              text_col = "white",
                              unit_category = "metric",
                              bar_cols = c("black", "white"),
                              width_hint = 0.35) +
  ggview::canvas(height = 10, width = 12)

mapa_principal

## Unindo os mapas ----

mapa_final <- ((br_map + pe_map) / mapa_principal) +
  patchwork::plot_layout(heights = c(2.5, 4),
                         widths = c(2, 2)) &
  theme(axis.text = element_text(size = 12.5),
        legend.position = "bottom")

mapa_final + ggview::canvas(width = 12.5, height = 12.5)

ggsave(filename = "mapa_cap2_2ver.png",
       height = 12.5, width = 12.5)
