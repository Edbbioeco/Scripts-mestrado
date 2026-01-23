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

# Carregando o tema ----

source("C:/Users/LENOVO/OneDrive/Documentos/funções/tema.R")

# Shapefile de distância dos corpos hídricos ----

source("distancia_corpos_hidricos_especies.R")

# Mapa ----

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
