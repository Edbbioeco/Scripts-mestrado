# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(writexl)

# Dados ----

## Parcelas ----

### Importar ----

parcelas <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/saltinho_ppbio_parcelas.shp")

### Visualizar ----

parcelas

ggplot() +
  geom_sf(data = parcelas)

## Borda ----

### Importar ----

borda <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/borda_saltinho_recortado.shp")

### Visualizar ----

borda

ggplot() +
  geom_sf(data = borda, color = "black") +
  geom_sf(data = parcelas, color = "red")

## Altitude de Saltinho ----

### Importar ----

alt <- terra::rast("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/altitude.tif")

### Visualizar ----

alt

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  geom_sf(data = borda, color = "red", fill = "transparent") +
  geom_sf(data = parcelas, color = "red") +
  scale_fill_viridis_c() +
  coord_sf(expand = FALSE)
