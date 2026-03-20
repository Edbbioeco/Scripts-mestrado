# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

# Dados ----

## Importando ----

coord <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/parcelas_saltinho.xlsx")

## Visualizando ----

coord

coord |> dplyr::glimpse()

# Calculando as coordenadas corrigidas ----

## Trilha 1 Parcela 3 ----

coord_trat <- coord |>
  dplyr::filter(Trilha == "1" & Parcela == 3) |>
  dplyr::select(Longitude, Latitude, `Azimute (graus)`, `Comprimento (m)`) |>
  dplyr::slice(-1)

coord_trat

coord_trat |> dplyr::glimpse()

## Convertendo as coordenadas para graus decimais ----

coord_trat <- coord_trat |>
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

coord_trat

coord_trat |> dplyr::glimpse()

## Calculando ----

convertendo_coords <- function(x){

  if(coord_trat$Longitude[x] |> is.na() | coord_trat$Latitude[x] |> is.na()){

    dest <- geosphere::destPoint(c(coord_trat$Longitude[x-1],
                                   coord_trat$Latitude[x-1]),
                                 coord_trat$`Azimute (graus)`[x-1],
                                 coord_trat$`Comprimento (m)`[x-1])

    coord_trat$Longitude[x] <<- dest[1]

    coord_trat$Latitude[x] <<- dest[2]

  }

}

purrr::map(2:nrow(coord_trat), convertendo_coords)

coord_trat

coord_trat |> dplyr::glimpse()
