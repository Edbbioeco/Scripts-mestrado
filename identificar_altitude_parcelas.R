# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

# Dados ----

## Altitude ----

### Importando ----

altitude <- readxl::read_xlsx("matriz_ambientais.xlsx")

### Visualizando ----

altitude

altitude |> as.data.frame()

altitude |> dplyr::glimpse()

## Shapefile das parcelas ----

### Importando ----

parcelas <- sf::st_read("coordenadas_parcelas_saltinho.shp")

### Visualizando ----

parcelas

ggplot() +
  geom_sf(data = parcelas, color = "black")

## PIS ----

### Importando ----

dados <- readxl::read_xlsx("dados_tratados_pis.xlsx")

### Visualizando ----

dados

dados |> as.data.frame()

dados |> dplyr::glimpse()
