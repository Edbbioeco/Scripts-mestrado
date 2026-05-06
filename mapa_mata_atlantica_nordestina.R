# Pacotes ----

library(sf)

library(tidyverse)

# Dados ----

## Shapefile dos Estados do Brasil ----

### Importar ----

br <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/aula_geoespacial/br.shp")

### Visualizar ----

br

ggplot() +
  geom_sf(data = br, color = "black")

## Shapefile dos estados do Nordeste ----

### Filtrar ----

ne <- br |>
  dplyr::filter(cod_rgn == 2)

### Visualizar ----

ne

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = ne, color = "black", fill = "goldenrod")
