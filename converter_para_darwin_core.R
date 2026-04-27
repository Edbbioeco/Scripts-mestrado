# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(sp)

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
                  stringr::str_replace("d", "°"),
                `Unidade Amostral` = c(paste0("T1P", 1:4),
                                       paste0("T2P", 1:4),
                                       paste0("T3P", 1:2),
                                       paste0("R", 1:2)))

coords_gms

## Conferindo se os taxons podem ser rastreados até seus IDs ----

taxon <- anuros |>
  dplyr::pull(Espécie) |>
  unique()

taxon

testar_id <- function(taxon){

  paste0("Tetando para: ", taxon) |>
    crayon::green() |>
    message()

  taxon |>
    rgbif::name_backbone_checklist() |>
    dplyr::pull(usageKey)

}

purrr::map(taxon, testar_id)

## Data frame das informações a serem copiadas ----

df_copy <- anuros |>
  dplyr::select(`Unidade Amostral`, Ordem:Espécie, Data) |>
  dplyr::left_join(coords_gms,
                   by = "Unidade Amostral") |>
  dplyr::filter(!Espécie |> is.na()) |>
  dplyr::mutate(Data = Data |> lubridate::ymd()) |>
  as.data.frame()

df_copy
