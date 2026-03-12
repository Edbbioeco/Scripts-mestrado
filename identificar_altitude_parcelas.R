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

pis <- readxl::read_xlsx("dados_tratados_pis.xlsx")

### Visualizando ----

pis

pis |> as.data.frame()

pis |> dplyr::glimpse()

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

## Adicionando os dados de parcela às informações de tombo ----

pis |>
  dplyr::mutate(`Unidade Amostral` = dplyr::case_when(
    Família == "Leptodactylidae" ~ "T1P3",
    Data == "20.viii.2025" & Família == "Craugastoridae" ~ "T2P1",
    Data == "21.viii.2025" ~ "R1",
    Data == "24.ix.2025" ~ "T2P1",
    Data == "25.ix.2025" ~ "T2P2")) |>
  dplyr::select(CHUFPE, Espécie, `Unidade Amostral`) |>
  as.data.frame()

## Unindo os dados de altitude e das parcelas ----

alt_parcela <- altitude |>
  dplyr::select(`Unidade Amostral`, Altitude) |>
  dplyr::left_join(parcelas_trat |>
                     dplyr::select(`Unidade Amostral`, geometry),
                   by = "Unidade Amostral") |>
  as.data.frame()

alt_parcela
