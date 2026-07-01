# Pacotes ----

library(sf)

library(tidyverse)

library(leaflet)

library(leaflet.extras)

library(leafem)

library(mapedit)

library(lwgeom)

# Dados ----

## Borda ----

### Importar ----

borda <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/borda_saltinho.shp")

### Visualizar ----

borda

ggplot() +
  geom_sf(data = borda, color = "black")

## Parcelas ----

### Importar ----

parcelas <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/saltinho_ppbio_parcelas.shp")

### Visualizar ----

parcelas

ggplot() +
  geom_sf(data = borda, color = "black") +
  geom_sf(data = parcelas, color = "black")

## Rodovias ----

### Criar mapa interativo ----

mapa <- leaflet::leaflet() |>
  leaflet::addProviderTiles(providers$Esri.WorldImagery) |>
  leaflet.extras::addDrawToolbar(
    targetGroup = "Draw",
    polylineOptions = TRUE,
    polygonOptions = TRUE,
    circleOptions = TRUE,
    rectangleOptions = TRUE,
    markerOptions = TRUE,
    circleMarkerOptions = TRUE,
    editOptions = leaflet.extras::editToolbarOptions()) |>
  leafem::addMouseCoordinates() |>
  leaflet::addPolygons(data = borda,
                       color = "red",
                       fillOpacity = 0) |>
  leaflet::addPolylines(data = parcelas,
                       color = "gold",
                       stroke = 0.1,
                       fillOpacity = 0)

mapa

### Editar o mapa ----

mapa_editado <- mapa |> mapedit::editMap()

mapa_editado <- mapa_editado$drawn

mapa_editado

ggplot() +
  geom_sf(data = borda, color = "black") +
  geom_sf(data = parcelas, color = "black") +
  geom_sf(data = mapa_editado, color = "black")

### Tratar ----

sf::st_crs(mapa_editado) <- 4674

### Exportar o shapefile ----

mapa_editado |> sf::st_write("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/rodovias_saltinho.shp",
                             append = TRUE)

# Recortar ----

## Recortar o shapefile ----

borda_recortado <- borda |>
  lwgeom::st_split(mapa_editado) |>
  sf::st_collection_extract("POLYGON") |>
  dplyr::mutate(id = paste0("Fragmento ", 1:dplyr::n()))

borda_recortado

ggplot() +
  geom_sf(data = borda_recortado, color = "black", aes(fill = id)) +
  geom_sf(data = parcelas, color = "black")

ggplot() +
  geom_sf(data = borda_recortado, color = "black", aes(fill = id)) +
  geom_sf(data = parcelas, color = "black") +
  facet_wrap(~id)

## Exportar ----

borda_recortado |> sf::st_write("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/borda_saltinho_recortado.shp")
