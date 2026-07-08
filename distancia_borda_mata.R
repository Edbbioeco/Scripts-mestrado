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

borda <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/borda_saltinho.shp")

### Visualizar ----

borda

ggplot() +
  geom_sf(data = borda, color = "black") +
  geom_sf(data = parcelas, color = "red")

## Rodovias ----

### Importar ----

rodovias <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/rodovias_saltinho.shp")

### Visualizar ----

rodovias

ggplot() +
  geom_sf(data = borda, color = "black") +
  geom_sf(data = parcelas, color = "red") +
  geom_sf(data = rodovias, color = "blue")

### Tratar o shapefile da borda ----

borda_trat <- borda |>
  lwgeom::st_split(rodovias) |>
  sf::st_collection_extract("POLYGON") |>
  dplyr::mutate(id = paste0("Fragmento ", 1:dplyr::n()))

borda_trat

ggplot() +
  geom_sf(data = borda_trat, color = "black") +
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

# Distância da borda ----

## Distância geodésica simples da borda ----

distancia_geodesica_borda <- parcelas |>
  sf::st_distance(borda[3, ] |>
                    sf::st_boundary())

distancia_geodesica_borda
