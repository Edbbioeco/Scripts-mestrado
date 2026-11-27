# Pacotes ----

library(readxl)

library(tidyverse)

library(betapart)

library(vegan)

library(performance)

library(broom)

library(flextable)

library(ggview)

library(reshape2)

library(glmmTMB)

library(DHARMa)

# Dados ----

## Abundância de espécies ----

### Importar ----

especies <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/matriz_composicao.xlsx")

### Visualizar ----

especies

especies |> dplyr::glimpse()

## Variáveis ambientais ----

### Importar ----

ambientais <-readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/matriz_ambientais.xlsx")

### Visualizar ----

ambientais

ambientais |> dplyr::glimpse()

# Data frame dos dados do modelo ----

## Dissimilaridade da composição ----

### Impportar funções ----

source("https://raw.githubusercontent.com/Edbbioeco/Scripts-mestrado/main/ruggiero_indices.R")

### Calcular unicidade ----

#### Bray-Curtis ----

unicidade_bray <- especies |>
  tibble::column_to_rownames(var = "Unidade Amostral") |>
  betapart::beta.pair.abund() |>
  purrr::pluck("beta.bray") |>
  uniqueness()

unicidade_bray

#### Ruggiero ----

unicade_rug <- especies |>
  tibble::column_to_rownames(var = "Unidade Amostral") |>
  beta_rlb_ab() |>
  uniqueness()

unicade_rug

## Unir os dados ----

df_modelos <- tibble::tibble(bray_uni = unicidade_bray,
                             rug_uni = unicade_rug,
                             canopy = ambientais$`Abertura do dossel`,
                             leaflitter = ambientais$`Altura da serrapilheira`,
                             water = ambientais$`Distância dos corpos hídricos`,
                             edge = ambientais$`Distância da Borda`,
                             elevation = ambientais$Altitude)

df_modelos

# Modelos lineares ----

## Criar os modelos ----

modelos_lm <- purrr::pmap(
  tidyr::expand_grid(indices = df_modelos[1:2] |> names(),
                     vars = df_modelos[3:7] |> names()),
  \(indices, vars){

    formula <- paste0(indices, " ~ ", vars) |>
      as.formula()

    lm(formula, data = df_modelos)

    },
  .progress = TRUE) |>
  purrr::set_names(tidyr::expand_grid(indices = df_modelos[1:2] |> names(),
                                      vars = df_modelos[3:7] |> names()) |>
                     dplyr::mutate(id = purrr::map2(
                       indices,
                       vars,
                       \(indices, vars){

                         paste0(indices, "-", vars)
                       }
                       )) |>
                     dplyr::pull(id))

modelos_lm

## Avaliar ajustes dos modelos ----

purrr::imap(
  modelos_lm,
  \(modelo, id){

    paste0("modelo ", id) |>
      crayon::blue() |>
      message()

    modelo |>
      performance::check_normality() |> print()

    modelo |>
      performance::check_heteroscedasticity() |> print()

    modelo |>
      performance::check_model(check = c("homogeneity",
                                         "qq",
                                         "normality")) |>
      print()

    },
  .progress = TRUE)

## Estatísticas do modelo ----

purrr::imap(
  modelos_lm,
  \(modelo, id){

    paste0("modelo ", id) |>
      crayon::blue() |>
      message()

    modelo |>
      summary() |>
      print()

    },
  .progress = TRUE)

## Extrair estatísticas como data frame ----

purrr::imap_dfr(
  modelos_lm,
  \(modelo, id){

    modelo |>
      broom::tidy() |>
      dplyr::mutate(modelo = id)

    },
  .progress = TRUE) |>
  dplyr::filter(!term |> stringr::str_detect("\\(") &
                  abs(statistic) >= qt(df = 10, p = 0.05, lower.tail = FALSE))
