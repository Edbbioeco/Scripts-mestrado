# Pacotes ----

library(sf)

library(tidyverse)

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
