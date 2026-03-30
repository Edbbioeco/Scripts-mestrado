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
                                   paste0("R", 1:2))))

parcelas_trat
