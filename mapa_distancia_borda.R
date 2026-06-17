# Pacotes ----

library(sf)

library(tidyverse)

library(ggspatial)

library(ggview)

# Dados ----

## Parcelas ----

### Importar ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizar ----

parcelas

ggplot() +
  geom_sf(data = parcelas)

## Borda de Saltinho ----

### Importar ----

borda <- sf::st_read("borda_saltinho.shp")

### Visualizar ----

borda

ggplot() +
  geom_sf(data = borda) +
  geom_sf(data = parcelas)

# Distância da borda ----

## Shapefile da distância mínima de cada parcela ----

dist_borda <- parcelas |>
  dplyr::filter(Trlh.Pr != "1-1") |>
  sf::st_centroid() |>
  sf::st_nearest_points(borda |>
                          sf::st_boundary())

dist_borda

ggplot() +
  geom_sf(data = borda) +
  geom_sf(data = dist_borda, color = "red") +
  geom_sf(data = parcelas, color = "blue")

## Gráfico ----

ggplot() +
  geom_sf(data = borda, color = "green4", fill = "transparent",
          linewidth = 1) +
  geom_sf(data = dist_borda, color = "red", linewidth = 1) +
  geom_sf(data = parcelas |>
            dplyr::filter(Trlh.Pr != "1-1"),
          color = "blue", linewidth = 1) +
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

ggsave(filename = "mapa_distancia_borda.png",
       height = 10, width = 12)
