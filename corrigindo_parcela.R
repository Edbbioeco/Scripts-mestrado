# Pacotes ----

library(readxl)

library(tidyverse)

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

coord_trat <- coord %>%
  dplyr::filter(Trilha == "1" & Parcela == 3)

coord_trat

coord_trat |> dplyr::glimpse()

## Calculando ----

convertendo_coords <- function(x){

  if(coord$Longitude[x] |> is.na() | coord$Latitude[x] |> is.na()){

    dest <- geosphere::destPoint(c(coord$Longitude[x-1],
                                   coord$Latitude[x-1]),
                                 coord$`Angulo de virada`[x-1],
                                 coord$`Distância (m)`[x-1])

    coord$Longitude[x] <<- dest[1]

    coord$Latitude[x] <<- dest[2]

  }

}

purrr::map(2:nrow(coord), convertendo_coords)
