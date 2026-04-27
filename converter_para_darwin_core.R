# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(geosphere)

library(rgbif)

library(writexl)

# Dados ----

## Modelo Darwin Core ----

### Importar ----

darwin_core <- readxl::read_xlsx("Template_lista_especies.xlsx")

### Visualizar ----

darwin_core

darwin_core |> dplyr::glimpse()

## Dados do levantamento de anuros ----

### Importar ----

anuros <- readxl::read_xlsx("levantamento_anuros.xlsx")

### Visualizar ----

anuros

anuros |> dplyr::glimpse()

## Coordenada das parcelas ----

### Importar ----

coords <-sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizar ------

coords

ggplot() +
  geom_sf(data = coords)

# Modelo ----

## Criar o modelo base ----

modelo <- darwin_core |>
  dplyr::mutate(baseOfRecord = "HumanObservation",
                occurrenceID = "",
                recordedBy = "",
                decimalLongitude = "",
                decimalLatitude = "",
                country = "") |>
  dplyr::relocate(recordedBy, .after = license) |>
  dplyr::relocate(decimalLongitude:country, .before = locality) |>
  dplyr::relocate(baseOfRecord:occurrenceID, .before = datasetName)

modelo

modelo |> dplyr::glimpse()

## Coordenadas das parcelas ----

coords_gms <- coords |>
  sf::st_centroid() |>
  sf::st_coordinates() |>
  as.data.frame() |>
  dplyr::rename("decimalLongitude" = X,
                "decimalLatitude" = Y) |>
  dplyr::mutate(decimalLongitude = decimalLongitude |>
                  sp::dd2dms(NS = FALSE) |>
                  as.character() |>
                  stringr::str_replace("d", "°"),
                decimalLatitude = decimalLatitude |>
                  sp::dd2dms(NS = TRUE) |>
                  as.character() |>
                  stringr::str_replace("d", "°"))

coords_gms
