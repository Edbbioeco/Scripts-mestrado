# Pacotes ----

library(sf)

library(tidyverse)

library(writexl)

# Dados ----

## Importando ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

## Visualizando ----

parcelas

ggplot() +
  geom_sf(data = parcelas, color = "black", linewidth = 1)

