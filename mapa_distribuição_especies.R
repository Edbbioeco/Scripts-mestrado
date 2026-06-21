# PAcotes ----

library(readxl)

library(tidyverse)

library(sf)

library(terra)

library(terra)

library(ggspatial)

library(ggview)

# Dados ----

## Dados de composição ----

### Importar ----

comp <- readxl::read_xlsx("matriz_composicao.xlsx")

### Visualizar ----

comp

comp |> dplyr::glimpse()

## Shapefile da borda da Mata de Saltinho ----

### Importar ----

borda <- sf::st_read("borda_saltinho.shp")

### Visualizar ----

borda

ggplot() +
  geom_sf(data = borda, color = "black")

## Shapefile dos corpos hídricos ----

### Importar ----

hid <- sf::st_read("corpos_hidricos_saltinho.gpkg")

### Visualizar ----

hid

ggplot() +
  geom_sf(data = borda, color = "black") +
  geom_sf(data = hid, color = "blue")

## Shapefile das parcelas ----

### Importar ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizar ----

parcelas

ggplot() +
  geom_sf(data = borda, color = "black") +
  geom_sf(data = hid, color = "blue") +
  geom_sf(data = parcelas, color = "black")

## Raster de altitude ----

### Importar ----

alt <- terra::rast("altitude.tif")

### Importar ----

alt

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  geom_sf(data = borda, color = "gold", fill = "transparent") +
  geom_sf(data = hid, color = "red", fill = "transparent") +
  geom_sf(data = parcelas, color = "gold", fill = "transparent")

# Mapa ----

## AbundÂncia por parcela ----

parcela_abund <- parcelas[-1, ] |>
  sf::st_centroid() |>
  dplyr::bind_cols(comp[, -1]) |>
  tidyr::pivot_longer(cols = 2:11,
                      names_to = "Espécie",
                      values_to = "Abundância")

parcela_abund

## Confecionar mapa ----

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  tidyterra::scale_fill_whitebox_c(palette = "arid",
                                   direction = -1,
                                   name = "Altitude",
                                   guide = guide_colorbar(order = 1,
                                                          title.position = "top",
                                                          title.hjust = 0.5,
                                                          barwidth = 15,
                                                          frame.colour = "black",
                                                          frame.linewidth = 1,
                                                          ticks.colour = "black",
                                                          ticks.linewidth = 1)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(-35.19509, -35.15463, 0.03)) +
  scale_y_continuous(expand = c(0, 0)) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = hid, aes(color = "Water streams"),
          fill = "blue",
          linewidth = 1) +
  geom_sf(data = borda, aes(color = "Forest environment"),
          linewidth = 1, fill = "transparent") +
  geom_sf(data = parcela_abund, aes(size = Abundância),
          color = "black",
          linewidth = 1) +
  scale_color_manual(values = c("Forest environment" = "darkgreen",
                                "Water streams" = "blue")) +
  scale_size_continuous(breaks = seq(0, 35, 5),
                        guide = guide_legend(order = 2,
                                             title = "Abundance",
                                             title.position = "top",
                                             title.hjust = 0.5),
                        range = c(1, 10)) +
  facet_wrap(~Espécie) +
  labs(x = NULL,
       y = NULL,
       colour = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        strip.text = ggtext::element_markdown(color = "black", size = 15),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = c(1, -0.05),
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.justification = c("right", "bottom"),
        legend.background = element_blank()) +
  ggview::canvas(height = 12, width = 16)
