# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

# Dados ----

## Anuros -----

### Importando ----

anuros <- readxl::read_xlsx("levantamento_anuros.xlsx")

### Visualizando ----

anuros

anuros |> as.data.frame()

anuros |> dplyr::glimpse()

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
