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

## Mata Atlântica ----

### Importar ----

ma <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/aula_geoespacial/biomas.shp") |>
  dplyr::filter(name_biome == "Mata Atlântica")

### Visualizar ----

ma

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = ne, color = "black", fill = "goldenrod") +
  geom_sf(data = ma, color = "darkgreen", fill = "transparent")

## Sahepfile das unidades de conservaão da Mata Altântica Nordestina ----

### Importar e recortar ----

uc <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/aula_geoespacial/unidade_conservacao.shp") |>
  sf::st_intersection(ma |>
                        sf::st_intersection(ne |>
                                              sf::st_union()))

### Visulizar ----

uc

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = ne, color = "black", fill = "goldenrod") +
  geom_sf(data = ma, color = "darkgreen", fill = "transparent") +
  geom_sf(data = uc, color = "darkred", fill = "darkred", alpha = 0.3)
