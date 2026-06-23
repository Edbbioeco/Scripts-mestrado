# PAcotes ----

library(terra)

library(tidyverse)

library(tidyterra)

library(ggspatial)

library(ggview)

# Raster de satélite ----

## Importar ----

saltinho <- terra::rast("saltinho.tif")

## Visualizar ----

saltinho

ggplot() +
  tidyterra::geom_spatraster_rgb(data = saltinho) +
  coord_sf(expand = FALSE)

# Mapa ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = saltinho) +
  coord_sf(expand = FALSE,
           label_graticule = "NSWE") +
  ggspatial::annotation_scale(localtion = "br",
                              text_face = "bold",
                              text_cex = 1.5,
                              bar_cols = c("black", "gold")) +
  theme(axis.text = element_text(color = "black", size = 20)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_apresentacao_defesa.png",
       height = 10,
       width = 12)
