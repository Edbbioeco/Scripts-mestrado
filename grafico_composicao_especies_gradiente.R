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

# Gráfico de barras -----

## Gráfico como as unidades amostrais ----

especies |>
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
    ordenaR::order_circle(gradient = var,
                          species = 2:11,
                          range = 5) +
    ggview::canvas(height = 10, width = 12)

  print(grafico)

  ggsave(filename = paste0("grafico_circulos_ordenar_",
                           var,
                           ".png"),
         height = 10,
         width = 12)

  assign(paste0("gg_", var),
         grafico,
         envir = globalenv())

}

var <- ambientais[c(2:3, 5, 7, 9)] |> names()

purrr::map(var, ordenar_especies)

design <- c(patchwork::area(1, 1), # A
            patchwork::area(1, 2), # B
            patchwork::area(2, 1), # C
            patchwork::area(2, 2), # D
            patchwork::area(3, 1)  # E → esquerda
            )

(`gg_Abertura do dossel` +
    gg_Altitude +
    `gg_Altura da serrapilheira` +
    `gg_Área das poças` +
    `gg_Distância dos corpos hídricos`) +
  patchwork::plot_layout(design = design) +
  ggview::canvas(height = 10, width = 12)

ggsave("grafico_composicao_especies.png",
       height = 10, width = 12)
