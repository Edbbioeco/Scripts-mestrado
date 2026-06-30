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

## Catálogo ----

catalogo <- CDSE::SearchCatalog(aoi = saltinho,
                                from = "2010-01-01",
                                to = "2026-05-01",
                                collection = "sentinel-2-l2a",
                                with_geometry = FALSE,
                                client = cliente,
                                filter = "eo:cloud_cover < 5")

catalogo

catalogo |> dplyr::glimpse()

## Evalscript ----

evalscript <- system.file("scripts",
                          "TrueColor.js",
                          package = "CDSE")

evalscript
