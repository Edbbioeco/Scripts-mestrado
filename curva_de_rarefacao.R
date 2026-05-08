# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggview)

# Dados ----

## Importar ----

comp <- readxl::read_xlsx("levantamento_anuros.xlsx")

## Visualizar ----

comp

comp |> dplyr::glimpse()

## Tratar ----

comp_trat <- comp |>
  dplyr::filter(Ordem == "Anura" &
                  !Epípeto == "natalensis" &
                  Gênero != "Frostius" &
                  Família != "Hylidae" &
                  `Unidade Amostral` != "T1P1") |>
  dplyr::summarise(Abundância = Abundância |> max(),
                   .by = c(Espécie, `Unidade Amostral`, Campanha)) |>
  dplyr::mutate(`Unidade Amostral` = paste0(Campanha, " ", `Unidade Amostral`)) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Abundância,
                     values_fill = 0)

comp_trat

# Curva de rarefação ----

## Calcular curva de rarefação ----

chao_df <- comp_trat |>
  dplyr::select(dplyr::where(is.numeric)) |>
  vegan::estaccumR(permutations = 1000) |>
  summary(display = c("S", "chao")) %>%
  .[[1]] |>
  as.data.frame() |>
  dplyr::rename("Sampling record" = N,
                "Richness" = S) |>
  dplyr::mutate(Tipo = "Observed") |>
  dplyr::bind_rows(comp_trat |>
                     dplyr::select(dplyr::where(is.numeric)) |>
                     vegan::estaccumR(permutations = 1000) |>
                     summary(display = c("S", "chao")) %>%
                     .[[2]] |>
                     as.data.frame() |>
                     dplyr::rename("Sampling record" = N,
                                   "Richness" = Chao) |>
                     dplyr::mutate(Tipo = "Estimated"))

chao_df
