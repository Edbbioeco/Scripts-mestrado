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

borda <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/borda_saltinho_completo.shp")

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

## Corpos hídricos ----

### Importar ----

hid <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/corpos_hidricos_saltinho.gpkg")

### Visualizar ----

hid

ggplot() +
  geom_sf(data = borda, color = "black") +
  geom_sf(data = parcelas, color = "red") +
  geom_sf(data = hid, color = "blue")

## Rodovias ----

### Criar mapa interativo ----

mapa <- leaflet::leaflet() |>
  leaflet::addProviderTiles(providers$Esri.WorldImagery) |>
  leaflet.extras::addDrawToolbar(
    targetGroup = "Draw",
    polylineOptions = leaflet.extras::drawPolylineOptions(),
    polygonOptions = leaflet.extras::drawPolygonOptions(),
    circleOptions = leaflet.extras::drawCircleOptions(),
    rectangleOptions = leaflet.extras::drawRectangleOptions(),
    markerOptions = leaflet.extras::drawMarkerOptions(),
    circleMarkerOptions = leaflet.extras::drawCircleMarkerOptions(),
    editOptions = leaflet.extras::editToolbarOptions()) |>
  leafem::addMouseCoordinates() |>
  leaflet::addPolygons(data = borda |>
                         sf::st_transform(crs = 4326),
                       color = "red",
                       fillOpacity = 0) |>
  leaflet::addPolylines(data = parcelas |>
                          sf::st_transform(crs = 4326),
                        color = "gold",
                        fillOpacity = 0) |>
  leaflet::addPolylines(data = hid |>
                          sf::st_as_sf() |>
                          sf::st_cast("LINESTRING") |>
                          sf::st_transform(4326) |>
                          sf::st_zm(drop = TRUE, what = "ZM"),
                       color = "blue")

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
  geom_sf(data = borda_recortado, color = "black") +
  geom_sf(data = parcelas, color = "red") +
  geom_sf(data = hid, color = "blue")

ggplot() +
  geom_sf(data = borda_recortado, color = "black") +
  geom_sf(data = parcelas, color = "red") +
  geom_sf(data = hid, color = "blue") +
  facet_wrap(~id)

## Exportar ----

borda_recortado |> sf::st_write("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/borda_saltinho_recortado.shp")
