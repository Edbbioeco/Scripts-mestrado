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

dist_hid <- parcelas |>
  dplyr::filter(Trlh.Pr != "1-1") |>
  sf::st_centroid() |>
  sf::st_nearest_points(corpos_hid |>
                          sf::st_boundary() |>
                          sf::st_cast("LINESTRING") |>
                          sf::st_union())

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
  geom_sf(data = dist_hid, color = "red", linewidth = 1) +
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

ggplot() +
  geom_sf(data = borda, aes(color = "Borda da Mata"),
          linewidth = 1, fill = "transparent") +
  geom_sf(data = corpos_hid, aes(color = "Corpos Hídricos"),
          linewidth = 1, fill = "royalblue") +
  geom_sf(data = saltinho, aes(color = "REBio Saltinho"),
          linewidth = 1, fill = "transparent") +
  geom_sf(data = parcelas, aes(color = "Parcela de Amostragem"),
          linewidth = 1, fill = "transparent") +
  geom_sf(data = linhas_conexao, aes(color = "Distância dos corpos hídricos"),
          linewidth = 1) +
  coord_sf(label_graticule = "NSWE", expand = FALSE) +
  scale_color_manual(values = c("Borda da Mata" = "darkgreen",
                                "Corpos Hídricos" = "royalblue",
                                "REBio Saltinho" = "red",
                                "Parcela de Amostragem" = "darkorange",
                                "Distância dos corpos hídricos"  = "blue"),
                     breaks = c("REBio Saltinho",
                                "Borda da Mata",
                                "Corpos Hídricos",
                                "Parcela de Amostragem",
                                "Distância dos corpos hídricos")) +
  labs(colour = NULL) +
  ggspatial::annotation_scale(location = "tr",
                              text_face = "bold",
                              text_cex = 2,
                              text_col = "black",
                              unit_category = "metric",
                              bar_cols = c("black", "gold")) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_corpos_hidricos_seminarios2.png", height = 10, width = 12)
