# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(ggspatial)

library(ggview)

library(patchwork)

# Dados ----

## Brasil ----

### Importando ----

br <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/br.shp")

### Visualizando ----

br

ggplot() +
  geom_sf(data = br, color = "black")

## Pernambuco ----

### Importando ----

pe <- br |>
  dplyr::filter(abbrv_s == "PE")

### Visualizando ----

pe

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = pe, color = "black", fill = "gold")

## Borda da mata ----

### Importando ----

borda <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/borda_saltinho.shp")

### Visualizando ----

borda

ggplot() +
  geom_sf(data = borda, color = "darkgreen", fill = "transparent", linewidth = 1)

## Parcelas de amostragem ----

### Importando ----

parcelas <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/saltinho_ppbio_parcelas.shp")

### Visualizando ----

parcelas

ggplot() +
  geom_sf(data = borda, color = "darkgreen", fill = "transparent", linewidth = 1) +
  geom_sf(data = parcelas, color = "red", linewidth = 1)

### Tratando ----

parcelas_trat <- parcelas |>
  dplyr::mutate(tipo = c(rep("Uniform sampling plot", 10),
                         rep("Riparian sampling plot", 2)),
                Trilha = dplyr::case_when(Trlh.Pr |>
                                            stringr::str_detect("1-") ~ "T1P",
                                          Trlh.Pr |>
                                            stringr::str_detect("2-") ~ "T2P",
                                          Trlh.Pr |>
                                            stringr::str_detect("3-") ~ "T3P",
                                          .default = "R"),
                `Unidade Amostral` = dplyr::case_when(
                  Trlh.Pr |>
                    stringr::str_detect("-1") ~ paste0(Trilha, 1),
                  Trlh.Pr |>
                    stringr::str_detect("-2") ~ paste0(Trilha, 2),
                  Trlh.Pr |>
                    stringr::str_detect("-3") ~ paste0(Trilha, 3),
                  Trlh.Pr |>
                    stringr::str_detect("-4") ~ paste0(Trilha, 4))) |>
  dplyr::select(-Trilha) |>
  dplyr::filter(!Trlh.Pr == "1-1") |>
  sf::st_centroid()

parcelas_trat

ggplot() +
  geom_sf(data = borda, color = "darkgreen", fill = "transparent", linewidth = 1) +
  geom_sf_label(data = parcelas_trat,
                aes(fill = tipo,
                    label = `Unidade Amostral`),
          color = "black", shape = 21, size = 3, linewidth = 1)

## Corpos hídricos ----

### Importando ----

corpos_hid <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/corpos_hidricos_saltinho.gpkg")

### Visualizando ----

corpos_hid

ggplot() +
  geom_sf(data = borda, color = "darkgreen", fill = "transparent", linewidth = 1) +
  geom_sf(data = parcelas, color = "red", linewidth = 1) +
  geom_sf(data = corpos_hid, color = "royalblue", fill = "transparent", linewidth = 1)

## Imagem de satélite ----

### Importando ----

saltinho_tif <- terra::rast("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/saltinhocdse_rgb.tif")

### Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = saltinho_tif) +
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
  geom_segment(aes(x = -35.1806, y = -8.726943,
                   xend = -35.4, yend = -8.55),
               arrow = arrow(length = unit(0.4, "cm"),
                             ends = "first",
                             type = "closed"),
               linewidth = 1.2,
               color = "black") +
  geom_label(aes(x = -35.4, y = -8.55,
                 label = "REBio Saltinho"),
             color = "black",
             fill = "tomato",
             size = 5,
             vjust = 0,
             label.padding = unit(0.3, "lines"),
             linewidth = 1) +
  ggspatial::coord_sf(label_graticule = "NE",
                      xlim = c(-36.3, -34.8),
                      ylim = c(-8.9, -7.4)) +
  scale_x_continuous(breaks = seq(-36.2, -34.8, 0.4)) +
  labs(x = NULL,
       y = NULL) +
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
          aes(color = "Forest environment"),
          linewidth = 1, fill = "transparent") +
  geom_sf(data = corpos_hid,
          aes(color = "Water streams"),
          linewidth = 1, fill = "transparent") +
  scale_fill_manual(values = c("Brazil" = "lightgray",
                               "Pernambuco" = "lightgoldenrod"),
                    breaks = c("Brazil", "Pernambuco")) +
  scale_color_manual(values = c("Forest environment" = "gold3",
                                "Water streams" = "royalblue",
                                "Uniform sampling plot" = "black",
                                "Riparian sampling plot" = "black"),
                     breaks = c("Forest environment", "Water streams",
                                "Uniform sampling plot", "Riparian sampling plot")) +
  guides(fill = guide_legend(order = 1, nrow = 2, title = NULL),
         color = guide_legend(order = 2, nrow = 2)) +
  labs(fill = NULL,
       color = NULL) +
  ggnewscale::new_scale_fill() +
  geom_sf_label(data = parcelas_trat,
                aes(fill = tipo, label = `Unidade Amostral`),
                color = "black", shape = 21, size = 5, stroke = 1) +
  scale_fill_manual(values = c("Uniform sampling plot" = "orange2",
                               "Riparian sampling plot" = "royalblue"),
                    breaks = c("Uniform sampling plot", "Riparian sampling plot")) +
  guides(fill = guide_legend(order = 3, nrow = 2)) +
  labs(fill = NULL,
       x = NULL,
       y = NULL) +
  ggspatial::coord_sf(label_graticule = "SEW",
                      xlim = c(-35.20179, -35.1561),
                      ylim = c(-8.745536, -8.71112),
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

# Mapa da apresentação ----

## Mapa do Brasil ----

br_map_ap <- ggplot() +
  geom_sf(data = br, color = "black", fill = "lightgray") +
  geom_sf(data = pe, color = "black", fill = "lightgoldenrod") +
  ggspatial::coord_sf(expand = FALSE,
                      label_graticule = "NWS") +
  theme(axis.text = element_text(size = 20)) +
  ggview::canvas(height = 10, width = 12)

br_map_ap

## Mapa de Pernambuco ----

pe_map_ap <- ggplot() +
  geom_sf(data = br, color = "black", fill = "lightgray", linewidth = 0.5) +
  geom_sf(data = pe, color = "black", fill = "lightgoldenrod", linewidth = 0.5) +
  geom_label(aes(x = -35.18123, y = -8.723923, label = "Saltinho"),
             color = "black",
             fill = "tomato",
             size = 3.5) +
  labs(x = NULL,
       y = NULL) +
  ggspatial::coord_sf(label_graticule = "NES",
                      xlim = c(-36.3, -34.8),
                      ylim = c(-8.9, -7.4)) +
  scale_x_continuous(breaks = seq(-36.2, -34.8, 0.4)) +
  theme(axis.text = element_text(size = 20)) +
  ggview::canvas(height = 10, width = 12)

pe_map_ap

## Mapa principal ----

mapa_principal_ap <- ggplot() +
  geom_sf(data = br, color = "black",
          aes(fill = "Brazil"), linewidth = 0.5) +
  geom_sf(data = pe, color = "black",
          aes(fill = "Pernambuco"), linewidth = 0.75) +
  tidyterra::geom_spatraster_rgb(data = saltinho_tif) +
  geom_sf(data = borda,
          aes(color = "Forest environment"),
          linewidth = 1, fill = "transparent") +
  geom_sf(data = corpos_hid,
          aes(color = "Water streams"),
          linewidth = 1, fill = "transparent") +
  scale_fill_manual(values = c("Brazil" = "lightgray",
                               "Pernambuco" = "lightgoldenrod"),
                    breaks = c("Brazil", "Pernambuco")) +
  scale_color_manual(values = c("Forest environment" = "gold3",
                                "Water streams" = "royalblue",
                                "Uniform sampling plot" = "black",
                                "Riparian sampling plot" = "black"),
                     breaks = c("Forest environment", "Water streams",
                                "Uniform sampling plot", "Riparian sampling plot")) +
  guides(fill = guide_legend(order = 1, nrow = 2, title = NULL),
         color = guide_legend(order = 2, nrow = 2)) +
  labs(fill = NULL,
       color = NULL) +
  ggnewscale::new_scale_fill() +
  geom_sf_label(data = parcelas_trat,
                aes(fill = tipo, label = `Unidade Amostral`),
                color = "black", shape = 21, size = 3.5, stroke = 1) +
  scale_fill_manual(values = c("Uniform sampling plot" = "orange2",
                               "Riparian sampling plot" = "royalblue"),
                    breaks = c("Uniform sampling plot", "Riparian sampling plot")) +
  guides(fill = guide_legend(order = 3, nrow = 2)) +
  labs(fill = NULL,
       x = NULL,
       y = NULL) +
  ggspatial::coord_sf(label_graticule = "NS",
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

mapa_principal_ap

## Unir mapas ----

(br_map_ap + mapa_principal_ap + pe_map_ap) +
  ggview::canvas(width = 16.5, height = 5.5)

ggsave(filename = "./apresentação/mapa_saltinho.png",
       width = 16.5, height = 5.5)
