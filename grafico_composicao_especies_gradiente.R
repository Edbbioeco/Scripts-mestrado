# Pacotes ----

library(readxl)

library(tidyverse)

library(ordenaR)

library(ggview)

library(patchwork)

# Dados ----

## Abundância de espécies ----

### Importando ----

especies <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/matriz_composicao.xlsx")

### Visualizando ----

especies

especies |> dplyr::glimpse()

## Variáveis ambientais ----

### Importando ----

ambientais <-readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/matriz_ambientais.xlsx")

### Visualizando ----

ambientais

ambientais |> dplyr::glimpse()

### Tratando ----

ambientais <- ambientais |>
  dplyr::rename("Canopy openness" = 3,
                "Leaf-litter depth" = 5,
                "Water stream distance" = 7,
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

## Gráficos das variáveis ambientais ----

graficos <- purrr::map(c("Leaf-litter depth",
                         "Canopy openness",
                         "Edge distance",
                         "Elevation",
                         "Water stream distance"),
                       purrr::in_parallel(\(var){

  grafico <- especies |>
    dplyr::left_join(ambientais,
                     by = "Unidade Amostral") |>
    dplyr::rename("Adenomera aff. hylaedactyla" = `Adenomera hylaedactyla`) |>
    ordenaR::order_circle(gradient = var,
                          species = 2:11,
                          range = 5) +
    labs(title = var) +
    theme(plot.title = element_text(hjust = 0.5,
                                    face = "bold",
                                    size = 20)) +
    ggview::canvas(height = 10, width = 12)

  print(grafico)

  assign(paste0("gg_", var),
         grafico,
         envir = globalenv())

  }),
  .progress = TRUE)

graficos

## Unindo os gráficos ----

graficos |>
  patchwork::wrap_plots() +
  patchwork::plot_layout(ncol = 2) +
  ggview::canvas(height = 10, width = 12)

ggsave("grafico_composicao_especies.png",
       height = 10, width = 12)

# Gráfico traduzido ----

## Importar e visualizar dados ambientais originais ----

ambientais <- readxl::read_xlsx("matriz_ambientais.xlsx") |>
  rename("Distância da borda" = 8,
         "Elevação" = 9)

ambientais

ambientais |> dplyr::glimpse()

## Gerar múltiplos gráficos ----

graficos <- purrr::map(
  c("Altura da serrapilheira",
    "Abertura do dossel",
    "Distância da borda",
    "Elevação",
    "Distância dos corpos hídricos"),
  purrr::in_parallel(\(var){

    especies |>
      dplyr::left_join(ambientais,
                       by = "Unidade Amostral") |>
      dplyr::rename("Adenomera aff. hylaedactyla" = `Adenomera hylaedactyla`) |>
      ordenaR::order_circle(gradient = var,
                            species = 2:11,
                            range = 5) +
      labs(title = var,
           y = "Abundância") +
      theme(plot.title = element_text(hjust = 0.5,
                                      face = "bold",
                                      size = 20)) +
      ggview::canvas(height = 10, width = 12)

    }
    ),
  .progress = TRUE) |>
  setNames(c("Altura da serrapilheira",
             "Abertura do dossel",
             "Distância da borda",
             "Elevação",
             "Distância dos corpos hídricos"))

graficos

## Unindo os gráficos ----

graficos |>
  patchwork::wrap_plots() +
  patchwork::plot_layout(ncol = 2) +
  ggview::canvas(height = 10, width = 12)

ggsave("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/grafico_composicao_especies.png",
       height = 10, width = 12)
