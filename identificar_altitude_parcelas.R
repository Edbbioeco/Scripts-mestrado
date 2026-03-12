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

# Alttitude das parcelas ----

## Tratando os dados das parcelas ----

parcelas_trat <- parcelas |>
  dplyr::mutate(Trilha = dplyr::if_else(Trilha == 3,
                                        "R",
                                        Trilha |> as.character()),
                `Unidade Amostral` = paste0("T", Trilha, "P", Parcela),
                `Unidade Amostral` = dplyr::case_when(Trilha == "R" ~ `Unidade Amostral` |>
                                                        stringr::str_remove_all("T|P"),
                                                      .default = `Unidade Amostral`))

parcelas_trat

## Unindo os dados de altitude e das parcelas ----

altitude |>
  dplyr::select(`Unidade Amostral`, Altitude) |>
  dplyr::left_join(parcelas_trat |>
                     dplyr::select(`Unidade Amostral`, geometry),
                   by = "Unidade Amostral") |>
  as.data.frame()
