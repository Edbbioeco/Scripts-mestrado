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

#### Hellinger ----

unicidade_hell <- especies |>
  tibble::column_to_rownames(var = "Unidade Amostral") |>
  vegan::decostand(method = "hellinger") |>
  betapart::beta.pair.abund() |>
  purrr::pluck("beta.bray") |>
  uniqueness()

unicidade_hell

#### Ruggiero ----

unicade_rug <- especies |>
  tibble::column_to_rownames(var = "Unidade Amostral") |>
  beta_rlb_ab() |>
  uniqueness()

unicade_rug

## Unir os dados ----

df_modelos <- tibble::tibble(bray_uni = unicidade_bray,
                             hell_uni = unicidade_hell,
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
  tidyr::expand_grid(indices = df_modelos[1:3] |> names(),
                     vars = df_modelos[4:8] |> names()),
  \(indices, vars){

    formula <- paste0(indices, " ~ ", vars) |>
      as.formula()

    lm(formula, data = df_modelos)

    },
  .progress = TRUE) |>
  purrr::set_names(tidyr::expand_grid(indices = df_modelos[1:3] |> names(),
                                      vars = df_modelos[4:8] |> names()) |>
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

## Extrair preditores significativos ----

vars_sig_lm <- purrr::imap_dfr(
  modelos_lm,
  \(modelo, id){

    modelo |>
      broom::tidy() |>
      dplyr::mutate(indice = id) |>
      dplyr::filter(!term |> stringr::str_detect("\\(") &
                      abs(statistic) >= qt(df = 10, p = 0.05, lower.tail = FALSE))

  },
  .progress = TRUE) |>
  dplyr::select(indice) |>
  tidyr::separate(col = 1,
                  into = c("tipo", "preditor"),
                  sep = "-") |>
  dplyr::mutate(signficativo = "sim")

vars_sig_lm

## Gráfico ----

purrr::map2(
    df_modelos[1:3] |> names(),
    c("Bray-Curtis", "Hellinger", "Ruggiero"),
    \(indice, titulo){

      df_modelos |>
        tidyr::pivot_longer(cols = 1:3,
                            names_to = "tipo",
                            values_to = "Uniqueness") |>
        tidyr::pivot_longer(cols = 1:5,
                            names_to = "preditor",
                            values_to = "valor preditor") |>
        dplyr::left_join(vars_sig_lm,
                         by = c("tipo", "preditor")) |>
        dplyr::mutate(signficativo = dplyr::case_when(

          signficativo |> is.na() ~ "não",
          .default = "sim"

          )) |>
        dplyr::filter(tipo == indice) |>
        ggplot(aes(`valor preditor`, Uniqueness)) +
        geom_point(size = 5) +
        geom_smooth(data = . %>%
                      dplyr::filter(signficativo == "sim"),
                    method = "lm",
                    linewidth = 1,
                    se = FALSE) +
        facet_wrap(~preditor, scales = "free_x") +
        labs(x = "Predictor value",
             y = "Uniqueness",
             title = titulo) +
        theme_classic() +
        theme(axis.text = element_text(color = "black", size = 20),
              axis.title = element_text(color = "black", size = 20),
              axis.line = element_line(color = "black", linewidth = 1),
              legend.text = element_text(color = "black", size = 20),
              legend.title = element_text(color = "black", size = 20),
              strip.text = element_text(color = "black", size = 20),
              strip.background = element_rect(color = "black", fill = "gray"),
              plot.title = element_text(color = "black", size = 30, hjust = 0.5))

      },
    .progress = TRUE)

# Modelos lineares generalizados ----

## Criar modelos ----

modelos_glm <- purrr::pmap(
  tidyr::expand_grid(indices = df_modelos[1:3] |> names(),
                     vars = df_modelos[4:8] |> names()),
  \(indices, vars){

    formula <- paste0(indices, " ~ ", vars) |>
      as.formula()

    glmmTMB::glmmTMB(formula, data = df_modelos,
                     family = glmmTMB::beta_family())

  },
  .progress = TRUE) |>
  purrr::set_names(tidyr::expand_grid(indices = df_modelos[1:3] |> names(),
                                      vars = df_modelos[4:8] |> names()) |>
                     dplyr::mutate(id = purrr::map2(
                       indices,
                       vars,
                       \(indices, vars){

                         paste0(indices, "-", vars)
                       }
                     )) |>
                     dplyr::pull(id))

modelos_glm

## Avaliar ajustes dos modelos ----

purrr::imap(
  modelos_glm,
  \(modelo, id){

    paste0("modelo ", id) |>
      crayon::blue() |>
      message()

    modelo |> DHARMa::simulateResiduals() |> print()

    modelo |> DHARMa::simulateResiduals(plot = TRUE)

  },
  .progress = TRUE)

## Estatísticas do modelo ----

purrr::imap(
  modelos_glm,
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
  modelos_glm,
  \(modelo, id){

    modelo |>
      marginaleffects::avg_comparisons() |>
      tibble::as_tibble() |>
      dplyr::select(3:4) |>
      dplyr::mutate(`z value` =  modelo |>
                      summary() |>
                      purrr::pluck("coefficients") |>
                      purrr::pluck("cond") |>
                      as.data.frame() |>
                      dplyr::slice(2) |>
                      dplyr::pull("z value"),
                    p = modelo |>
                      summary() |>
                      purrr::pluck("coefficients") |>
                      purrr::pluck("cond") |>
                      as.data.frame() |>
                        dplyr::slice(2) |>
                      dplyr::pull("Pr(>|z|)"),
                    R2 = modelo |>
                      performance::r2_ferrari() |>
                      as.numeric(),
                    modelo = id)

  },
  .progress = TRUE) |>
  tibble::rownames_to_column(var = "term") |>
  dplyr::filter(abs(`z value`) >= 1.96)

## Extrair preditores significativos ----

vars_sig_glm <- purrr::imap_dfr(
  modelos_glm,
  \(modelo, id){

    modelo |>
      summary() |>
      purrr::pluck("coefficients") |>
      purrr::pluck("cond") |>
      as.data.frame() |>
      dplyr::mutate(R2 = modelo |>
                      performance::r2_ferrari() |>
                      as.numeric(),
                    tipo = id)

  },
  .progress = TRUE) |>
  tibble::rownames_to_column(var = "preditor") |>
  dplyr::filter(!preditor |> stringr::str_detect("\\(") &
                  abs(`z value`) >= 1.96) |>
  dplyr::mutate(significativo = "sim",
                preditor = preditor |>
                  stringr::str_remove("\\.\\.\\.[0-9]+$")) |>
  dplyr::select(tipo, significativo) |>
  tidyr::separate(col = 1,
                  into = c("tipo", "preditor"),
                  sep = "-")

vars_sig_glm

## Gráfico ----

purrr::map2(
  df_modelos[1:3] |> names(),
  c("Bray-Curtis", "Hellinger", "Ruggiero"),
  \(indice, titulo){

    df_modelos |>
      tidyr::pivot_longer(cols = 1:3,
                          names_to = "tipo",
                          values_to = "Uniqueness") |>
      tidyr::pivot_longer(cols = 1:5,
                          names_to = "preditor",
                          values_to = "valor preditor") |>
      dplyr::left_join(vars_sig_glm,
                       by = c("tipo", "preditor")) |>
      dplyr::mutate(significativo = dplyr::case_when(

        significativo |> is.na() ~ "não",
        .default = "sim"

      )) |>
      dplyr::filter(tipo == indice) |>
      ggplot(aes(`valor preditor`, Uniqueness)) +
      geom_point(size = 5) +
      geom_smooth(data = . %>%
                    dplyr::filter(significativo == "sim"),
                  method = "lm",
                  linewidth = 1,
                  se = FALSE) +
      facet_wrap(~preditor, scales = "free_x") +
      labs(x = "Predictor value",
           y = "Uniqueness",
           title = titulo) +
      theme_classic() +
      theme(axis.text = element_text(color = "black", size = 20),
            axis.title = element_text(color = "black", size = 20),
            axis.line = element_line(color = "black", linewidth = 1),
            legend.text = element_text(color = "black", size = 20),
            legend.title = element_text(color = "black", size = 20),
            strip.text = element_text(color = "black", size = 20),
            strip.background = element_rect(color = "black", fill = "gray"),
            plot.title = element_text(color = "black", size = 30, hjust = 0.5))

  },
  .progress = TRUE)
