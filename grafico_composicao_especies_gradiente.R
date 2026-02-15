# Pacotes ----

library(readxl)

library(tidyverse)

library(ordenaR)

library(ggview)

library(patchwork)

# Dados ----

## Abundância de espécies ----

### Importando ----

especies <- readxl::read_xlsx("matriz_composicao.xlsx")

### Visualizando ----

especies

especies |> dplyr::glimpse()

## Variáveis ambientais ----

### Importando ----

ambientais <-readxl::read_xlsx("matriz_ambientais.xlsx")

### Visualizando ----

ambientais

ambientais |> dplyr::glimpse()

### Tratando ----

ambientais <- ambientais |>
  dplyr::rename("Canopy openness" = 3,
                "Leaf-litter depth" = 5,
                "Hydric stream distance" = 7,
                "Edge distance" = 8,
                "Elevation" = 9)

ambientais

# Gráfico de barras -----

## Gráfico como as unidades amostrais ----

especies |>
  dplyr::rename("Adenomera aff. hylaedactyla" = `Adenomera hylaedactyla`) |>
  ordenaR::order_circle(gradient = "Unidade Amostral",
                        species = 2:11,
                        direct = FALSE,
                        range = 20) +
  ggview::canvas(height = 10, width = 12)

## Gráfico das variáveis ambientais ----

ordenar_especies <- function(var){

  grafico <- especies |>
    dplyr::left_join(ambientais,
                     by = "Unidade Amostral") |>
    dplyr::rename("Adenomera aff. hylaedactyla" = `Adenomera hylaedactyla`) |>
    ordenaR::order_circle(gradient = var,
                          species = 2:11,
                          range = 5) +
    ggview::canvas(height = 10, width = 12)

  print(grafico)

  assign(paste0("gg_", var),
         grafico,
         envir = globalenv())

}

var <- ambientais[c(3, 5, 7:9)] |> names()

purrr::map(var, ordenar_especies)

design <- c(patchwork::area(1, 1),
            patchwork::area(1, 2),
            patchwork::area(2, 1),
            patchwork::area(2, 2),
            patchwork::area(3, 1))

(`gg_Leaf-litter depth` +
    `gg_Canopy openness` +
    `gg_Edge distance` +
    `gg_Elevation` +
    `gg_Hydric stream distance`) +
  patchwork::plot_layout(design = design) +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold",
                                size = 25)) &
  ggview::canvas(height = 10, width = 12)

ggsave("grafico_composicao_especies.png",
       height = 10, width = 12)
