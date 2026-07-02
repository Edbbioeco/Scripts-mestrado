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
