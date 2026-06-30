# Pacotes ----

library(sf)

library(tidyverse)

library(CDSE)

library(tidyterra)

library(terra)

# Dados ----

## Importando ----

saltinho <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/Saltinho.shp")

## Visualizando ----

saltinho |>
  ggplot() +
  geom_sf(color = "black", linewidth = 1)

# Imgem de satélite ----

## Autenticar cliente CDSE ----

cliente <- CDSE::GetOAuthClient(id = Sys.getenv("CDSE_ID"),
                                secret = Sys.getenv("CDSE_SECRET"))

cliente
