# Pacotes ----

library(sf)

library(tidyverse)

library(geobr)

library(terra)

library(tidyterra)

library(ggspatial)

library(ggview)

library(ggmagnify)

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

## Shapefile Saltinho ----

### Importnado ----

saltinho <- sf::st_read("Saltinho.shp")

### Visualizando ----

saltinho

ggplot() +
  geom_sf(data = saltinho, color = "red", linewidth = 1) +
  geom_sf(data = parcelas_trat, aes(color = tipo), linewidth = 1)

## Mata ----

### Importando ----

borda <- sf::st_read("borda_saltinho.shp")

### Visualizando ----

borda

ggplot() +
  geom_sf(data = borda, color = "black", linewidth = 1)

## Hidric bodies ----

### Importando ----

corpos_hid <- sf::st_read("corpos_hidricos_saltinho.gpkg")

### Visualizando ----

corpos_hid

ggplot() +
  geom_sf(data = borda, color = "black", linewidth = 1) +
  geom_sf(data = corpos_hid, color = "blue", linewidth = 1)

## Brasil ----

### Importando ----

br <- geobr::read_state(year = 2019)

### Visualizando ----

br

ggplot() +
  geom_sf(data = br, color = "black", linewidth = 1)

## Pernambuco ----

### Importando ----

pe <- br |>
  dplyr::filter(name_state == "Pernambuco")

### Visualizando ----

pe

ggplot() +
  geom_sf(data = br, color = "black", fill = "white", linewidth = 1) +
  geom_sf(data = pe, color = "black", fill = "lightgoldenrod", linewidth = 1)

## Raster de saltinho ----

### Importando ----

saltinho_rast <- terra::rast("saltinho.tif")

### Visualizando ----

saltinho_rast

ggplot() +
  tidyterra::geom_spatraster_rgb(data = saltinho_rast) +
  geom_sf(data = saltinho, color = "red", linewidth = 1, fill = "transparent") +
  geom_sf(data = parcelas, color = "gold4", linewidth = 1, fill = "transparent")

# Mapa ----

## Setando tema ----

source("C:/Users/LENOVO/OneDrive/Documentos/funções/tema.R")

## Mapa principal ----

mapa_principal <- ggplot() +
  geom_sf(data = br, color = "black",
          aes(fill = "Brazil"), linewidth = 1) +
  geom_sf(data = pe, color = "black",
          aes(fill = "Pernambuco"), linewidth = 1) +
  tidyterra::geom_spatraster_rgb(data = saltinho_rast) +
  geom_sf(data = borda,
          aes(color = "Native Forest"),
          linewidth = 1, fill = "transparent") +
  geom_sf(data = corpos_hid,
          aes(color = "Hidric bodies"),
          linewidth = 1, fill = "transparent") +
  geom_sf(data = saltinho,
          aes(color = "REBio Saltinho"),
          linewidth = 1, fill = "transparent") +
  geom_sf(data = parcelas_trat,
          aes(color = tipo),
          linewidth = 1, fill = "transparent") +
  coord_sf(label_graticule = "NSEW",
           xlim = c(-35.20319, -35.15696),
           ylim = c(-8.744113, -8.710025),
           expand = FALSE) +
  labs(fill = NULL,
       color = NULL) +
  scale_fill_manual(values = c("white",
                               "lightgoldenrod")) +
  scale_color_manual(values = c("Native Forest" = "gold3",
                                "REBio Saltinho" = "red",
                                "Hidric bodies" = "royalblue",
                                "Uniform Sample" = "orange2",
                                "Riparian Sample" = "purple"),
                     breaks = c("Native Forest", "Hidric bodies", "REBio Saltinho",
                                "Uniform Sample", "Riparian Sample")) +
  ggspatial::annotation_scale(location = "br",
                              text_face = "bold",
                              text_cex = 2,
                              text_col = "black",
                              unit_category = "metric",
                              bar_cols = c("black", "gold"),
                              width_hint = 0.35) +
  ggview::canvas(height = 10, width = 12)

mapa_principal

## Mapa paralelo ----

mapa_paralelo <- ggplot(data = br) +
  geom_sf(color = "black", fill = "white", linewidth = 0.5) +
  geom_sf(data = pe, color = "black", fill = "lightgoldenrod", linewidth = 0.5) +
  tidyterra::geom_spatraster_rgb(data = saltinho_rast) +
  scale_x_continuous(limits = c(-74, -30)) +
  ggmagnify::geom_magnify(from = c(-35.20319, -35.15696, -8.744113, -8.710025),
                          to = c(-41,
                                 (-41 + 0.04623 * 200),
                                 -5,
                                 (-5 + 0.034088 * 200)),
                          linewidth = 1,
                          shadow = TRUE,
                          colour = "darkred",
                          proj.fill = alpha("red", 0.5),
                          expand = FALSE) +
  theme_void() +
  ggview::canvas(height = 10, width = 12)

mapa_paralelo

## Mapa final

cowplot::ggdraw(mapa_principal) +
  cowplot::draw_plot(mapa_paralelo,
                     x = 0.6,
                     y = 0.55,
                     height = 0.35,
                     width = 0.35) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_dissertacao_cap1.png", height = 10, width = 12)
