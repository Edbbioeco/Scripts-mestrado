# Pacotes ----

library(sf)

library(tidyverse)

# Dados ----

## Borda ----

### Importar ----

borda <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/borda_saltinho.shp")

### Visualizar ----

borda

ggplot() +
  geom_sf(data = borda, color = "black")

## Rodovias ----

### Importar ----

rodovias <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/SNV_202604A.shp")

### Visualizar ----

rodovias

ggplot() +
  geom_sf(data = rodovias, color = "black")

