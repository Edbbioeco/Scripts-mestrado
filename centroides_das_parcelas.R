# Pacotes ----

library(sf)

library(tidyverse)

library(writexl)

# Dados ----

## Importando ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

## Visualizando ----

parcelas

ggplot() +
  geom_sf(data = parcelas, color = "black", linewidth = 1)

## Tratando ----

parcelas_trat <- parcelas |>
  dplyr::rename("Parcela" = Trlh.Pr) |>
  dplyr::mutate(Parcela = paste0(c(paste0("T", rep(c(1, 2, 3),
                                                   times = c(4, 4, 2))),
                                   paste0("R", 1:2)),
                                 c(paste0("P", rep(1:4,
                                                   times = 2)),
                                   paste0("P", 1:2),
                                   paste0("R", 1:2)))) |>
  dplyr::filter(!Parcela == "T1P1")

parcelas_trat

# Centroides ----

## Calculando os centroides ----

centroides <- parcelas_trat |>
  sf::st_centroid()

centroides

ggplot() +
  geom_sf(data = centroides, color = "red", size = 2)

## Transformando em data frame ----

centroides_df <- centroides |>
  sf::st_coordinates() |>
  as.data.frame() |>
  dplyr::rename("Longitude" = X, "Latitude" = Y) |>
  dplyr::mutate(Parcela = parcelas_trat$Parcela)

centroides_df

## Exportando ----

centroides_df |> writexl::write_xlsx("centroides_parcelas.xlsx")
