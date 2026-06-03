# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(terra)

library(tidyterra)

library(vegan)

library(ggspatial)

library(ggview)

# Dados ----

## Matriz de composição ----

### Importar ----

comp <- readxl::read_xlsx("matriz_composicao.xlsx")

### Visualizar ----

comp

comp |> dplyr::glimpse()

## Shapefile da borda de Saltinho ----

### Importar ----

borda <- sf::st_read("borda_saltinho.shp")

### Visualizar ----

borda

ggplot() +
  geom_sf(data = borda, color = "black", linewidth = 1)

## Coordenadas das parcelas ----

### Importar ----

parcelas <- sf::st_read("coordenadas_parcelas_saltinho.shp")

### Visualizar ----

parcelas

ggplot() +
  geom_sf(data = borda, color = "black", linewidth = 1) +
  geom_sf(data = parcelas, color = "black", linewidth = 1)

## Corpos Hídricos ----

### Importar ----

corpos_hid <- sf::st_read("corpos_hidricos_saltinho.gpkg")

### Visualizar ----

corpos_hid

ggplot() +
  geom_sf(data = borda, color = "black", linewidth = 1) +
  geom_sf(data = corpos_hid, color = "black", linewidth = 1) +
  geom_sf(data = parcelas, color = "black", linewidth = 1)

## Altitude ----

### Importar ----

alt <- terra::rast("altitude.tif")

### Visualizar ----

alt

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  geom_sf(data = borda, color = "black", linewidth = 1, fill = "transparent") +
  geom_sf(data = parcelas, color = "black", linewidth = 1) +
  scale_fill_viridis_c()

# Diversidade taxonômica ----

## Calculara a diversidade taoxnômica ----

div_tax <- comp |>
  tibble::column_to_rownames(var = "Unidade Amostral") |>
  vegan::renyi(scales = 0:1, hill = TRUE) |>
  tibble::rownames_to_column() |>
  dplyr::rename("Sampling Units" = rowname,
                "Richness" = `0`,
                "Alpha Diversity" = `1`)

div_tax

## Atribuindo estes valores ao shapefile das coordenadas ----

parcelas_div <- parcelas |>
  dplyr::slice(-1) |>
  dplyr::bind_cols(div_tax)

parcelas_div

# Mapa ----

## Mapa de riqueza ----

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  geom_sf(data = borda,
          aes(color = "Forest environment"),
          linewidth = 1,
          fill = "transparent") +
  geom_sf(data = corpos_hid,
          aes(color = "Hydric Streams"),
          linewidth = 1,
          fill = "transparent") +
  geom_sf(data = parcelas_div,
          aes(size = Richness),
          fill = "forestgreen",
          color = "black",
          shape = 21,
          stroke = 1) +
  tidyterra::scale_fill_whitebox_c(palette = "arid",
                                   direction = -1,
                                   name = "Altitude",
                                   guide = guide_colorbar(order = 1,
                                                          title.position = "top",
                                                          title.hjust = 0.5,
                                                          barwidth = 20,
                                                          frame.colour = "black",
                                                          frame.linewidth = 1,
                                                          ticks.colour = "black",
                                                          ticks.linewidth = 1)) +
  scale_color_manual(values = c("Forest environment" = "darkgreen",
                                "Hydric Streams" = "blue"),
                     breaks = c("Forest environment",
                                "Hydric Streams"),
                     guide = guide_legend(order = 2)) +
  scale_size_continuous(breaks = seq(4, 6, 1),
                        guide = guide_legend(order = 3,
                                             title.position = "top",
                                             title.hjust = 0.5,)) +
  scale_x_continuous(expand = FALSE,
                     breaks = seq(-35.19509, -35.15463, 0.03)) +
  scale_y_continuous(expand = FALSE) +
  ggspatial::annotation_scale(location = "bl",
                              bar_cols = c("black", "gold"),
                              text_cex = 2.5) +
  labs(x = NULL,
       y = NULL,
       colour = NULL) +
  coord_sf(label_graticule = "NSWE") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 17.5),
        legend.text = element_text(color = "black", size = 17.5),
        legend.title = element_text(color = "black", size = 17.5),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "bottom",
        legend.background = element_blank()) +
  ggview::canvas(height = 10, width = 12)

## Mapa de diversidade taxonômica ----

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  geom_sf(data = borda,
          aes(color = "Forest environment"),
          linewidth = 1,
          fill = "transparent") +
  geom_sf(data = corpos_hid,
          aes(color = "Water streams"),
          linewidth = 1,
          fill = "transparent") +
  geom_sf(data = parcelas_div,
          aes(size = `Alpha Diversity`),
          fill = "forestgreen",
          color = "black",
          shape = 21,
          stroke = 1) +
  tidyterra::scale_fill_whitebox_c(palette = "arid",
                                   direction = -1,
                                   name = "Altitude",
                                   guide = guide_colorbar(order = 1,
                                                          title.position = "top",
                                                          title.hjust = 0.5,
                                                          barwidth = 20,
                                                          frame.colour = "black",
                                                          frame.linewidth = 1,
                                                          ticks.colour = "black",
                                                          ticks.linewidth = 1)) +
  scale_color_manual(values = c("Forest environment" = "darkgreen",
                                "Water streams" = "blue"),
                     breaks = c("Forest environment",
                                "Water streams"),
                     guide = guide_legend(order = 2)) +
  scale_size_continuous(breaks = seq(2.5, 4.5, 0.25),
                        guide = guide_legend(order = 3,
                                             title.position = "top",
                                             title.hjust = 0.5),
                        range = c(1, 10)) +
  scale_x_continuous(expand = FALSE,
                     breaks = seq(-35.19509, -35.15463, 0.03)) +
  scale_y_continuous(expand = FALSE) +
  ggspatial::annotation_scale(location = "bl",
                              bar_cols = c("black", "gold"),
                              text_cex = 2.5) +
  labs(x = NULL,
       y = NULL,
       colour = NULL) +
  coord_sf(label_graticule = "NSWE") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 17.5),
        legend.text = element_text(color = "black", size = 17.5),
        legend.title = element_text(color = "black", size = 17.5),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "bottom",
        legend.background = element_blank()) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_valores_diversidade_cap2.png",
       height = 10, width = 12)
