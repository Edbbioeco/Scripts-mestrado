# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

# Dados ----

## Shapefile dos Estados do Brasil ----

### Importar ----

br <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/aula_geoespacial/br.shp")

### Visualizar ----

br

ggplot() +
  geom_sf(data = br, color = "black")
