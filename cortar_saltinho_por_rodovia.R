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

rodovias <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/GEOFT_TRECHO_RODOVIARIO_ESTADUAL.shp")

### Tratar ----

rodovias_pe <- rodovias |>
  dplyr::filter(UF_SIGLA == "PE")

### Visualizar ----

rodovias_pe

ggplot() +
  geom_sf(data = rodovias_pe, color = "black") +
  geom_sf(data = borda, color = "red", fill = "transparent")

ggplot() +
  geom_sf(data = rodovias_pe, color = "black") +
  geom_sf(data = borda, color = "red", fill = "transparent") +
  coord_sf(xlim = c(-35.20064, -35.15706),
           ylim = c(-8.744357, -8.712321))

# Recortar a mata de Saltinho ----

## Cortar ----

borda |>
  lwgeom::st_split(rodovias) |>
  plot()
