# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

# Dados ----

## Parcelas ----

### Importando ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizando ----

parcelas

ggplot() +
  geom_sf(data = parcelas)

## Shapefile Saltinho ----

### Importando ----

saltinho_shp <- sf::st_read("Saltinho.shp")

### Visualizando ----

ggplot() +
  geom_sf(data = saltinho_shp, color = "red", fill = "transparent") +
  geom_sf(data = parcelas)

## Imagem de satélite ----

### Importando ----

saltinho_tif <- terra::rast("saltinho.tif")

### Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = saltinho_tif) +
  geom_sf(data = saltinho_shp, color = "red", fill = "transparent") +
  geom_sf(data = parcelas, color = "gold")

# Mapa ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = saltinho_tif) +
  geom_sf(data = saltinho_shp,
          aes(color = "REBio Saltinho"),
          fill = "transparent",
          linewidth = 1) +
  geom_sf(data = parcelas,
          aes(color = "Parcelas"),
          linewidth = 1) +
  scale_color_manual(values = c("REBio Saltinho" = "red",
                                "Parcelas" = "gold")) +
  coord_sf(label_graticule = "NSEW") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(colour = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12.5),
        legend.text = element_text(color = "black", size = 12.5),
        legend.position = "bottom")

ggsave(filename = "mapa_parcelas_saltinho.png", height = 10, width = 12)
