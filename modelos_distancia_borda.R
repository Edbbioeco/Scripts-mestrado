# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(janitor)

library(performance)

library(ggtext)

library(ggview)

library(glmmTMB)

library(DHARMa)

library(ordenaR)

library(crayon)

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

# Diversidade alfa ----

## Calculando a diversidade ----

div_alfa <- especies |>
  tibble::column_to_rownames(var = "Unidade Amostral") |>
  vegan::renyi(scales = 1, hill = TRUE) |>
  as.numeric()

div_alfa

## Gerando o dataframe para o modelo linear ----

df_alfa <- ambientais |>
  dplyr::mutate(`Q = 1` = div_alfa)

df_alfa

df_alfa |> dplyr::glimpse()

# Diversidade beta e variação ambiental ----

## Calculando a diversidade beta ----

dis_beta <- especies |>
  tibble::column_to_rownames(var = "Unidade Amostral") |>
  vegan::vegdist() |>
  as.numeric()

dis_beta

## Calculando a variação ambiental ----

dis_borda <- ambientais |>
  dplyr::select(`Distância da Borda`) |>
  vegan::vegdist(method = "euclidean") |>
  as.numeric()

dis_borda

## Gerando o dataframe para o modelo linear ----

df_beta <- tibble::tibble(Composição = dis_beta,
                          `Dissimilaridade ambiental` = dis_borda)

df_beta

df_beta |> dplyr::glimpse()

# Abundância das espécies ----

## Calculando a abundância das espécies ----

abund <- especies |>
  dplyr::select(1:4)

abund

## Gerando o dataframe para o modelo linear ----

df_abund <- ambientais |>
  dplyr::select(`Unidade Amostral`, `Distância da Borda`) |>
  dplyr::left_join(abund,
                   by = "Unidade Amostral") |>
  janitor::clean_names()

df_abund

df_abund |> dplyr::glimpse()

# Modelos lineares de diversidade alfa ----

## Criando o modelo ----

modelo_alfa <- lm(`Q = 1` ~ `Distância da Borda`,
                  data = df_alfa)

## Pressupostos do modelo ----

modelo_alfa |> performance::check_model(check = c("homogeneity",
                                                  "qq",
                                                  "normality"))

modelo_alfa |> performance::check_heteroscedasticity()

modelo_alfa |> performance::check_normality()

## Estatísticas do modelo ----

sts_modelo_alfa <- modelo_alfa |> summary()

sts_modelo_alfa

## Dataframe das estatísticas do modelo ----

sts_grafico_alfa <- tibble::tibble(sts = paste0("β1 ± EP = ",
                                                sts_modelo_alfa$coefficients[2, 1] |> round(3),
                                                " ± ",
                                                sts_modelo_alfa$coefficients[2, 2] |> round(4),
                                                "<br>t = ",
                                                sts_modelo_alfa$coefficients[2, 3] |> round(2),
                                                ", p = ",
                                                sts_modelo_alfa$coefficients[2, 4] |> round(3),
                                                "<br> F<sub>",
                                                sts_modelo_alfa$fstatistic[2] |>
                                                  round(1),
                                                ", ",
                                                sts_modelo_alfa$fstatistic[3] |>
                                                  round(1),
                                                "</sub> = ",
                                                sts_modelo_alfa$fstatistic[1] |>
                                                  round(2),
                                                ", p = ",
                                                pf(sts_modelo_alfa$fstatistic[1],
                                                   sts_modelo_alfa$fstatistic[2],
                                                   sts_modelo_alfa$fstatistic[3],
                                                   lower.tail = FALSE) |>
                                                  round(2),
                                                ", R² = ",
                                                sts_modelo_alfa$adj.r.squared |>
                                                  round(2)),
                                   `Q = 1` = 4,
                                   `Distância da Borda` = df_alfa |>
                                     dplyr::pull(`Distância da Borda`) |>
                                     mean()) |>
  as.data.frame()

sts_grafico_alfa

## Gráfico ----

df_alfa |>
  ggplot(aes(`Distância da Borda`, `Q = 1`)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm",
              se = FALSE) +
  ggtext::geom_richtext(data = sts_grafico_alfa,
                        aes(`Distância da Borda`, `Q = 1`, label = sts),
                        label.colour = NA,
                        fill = NA,
                        size = 7.5) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 25),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 19),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "none",
        title = element_text(color = "black", size = 25),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "grafico_pontos_q1_distancia_borda.png",
       height = 10, width = 12)

# Modelos lineares de diversidade beta ----

## Criando o modelo ----

modelo_beta <- glmmTMB::glmmTMB(Composição ~ `Dissimilaridade ambiental`,
                                df_beta,
                                family = glmmTMB::beta_family())

## Pressupostos do modelo ----

modelo_beta |> DHARMa::simulateResiduals(plot = TRUE)

## Estatísticas do modelo ----

sts_modelo_beta <- modelo_beta |> summary()

sts_modelo_beta

r2_beta <- modelo_beta |> performance::r2_mcfadden()

r2_beta

## Dataframe das estatísticas do modelo ----

sts_grafico_beta <- tibble::tibble(sts = paste0("β1 ± EP = ",
                                                sts_modelo_beta$coefficients$cond[2, 1] |> round(5),
                                                " ± ",
                                                sts_modelo_beta$coefficients$cond[2, 2] |> round(4),
                                                "<br>z = ",
                                                sts_modelo_beta$coefficients$cond[2, 3] |> round(2),
                                                ", p < 0.01, pseudo-R² = ",
                                                r2_beta[[2]] |> round(2)),
                                   Composição = 0.6,
                                   `Dissimilaridade ambiental` = df_beta |>
                                     dplyr::pull(`Dissimilaridade ambiental`) |>
                                     mean()) |>
  as.data.frame()

sts_grafico_beta

## Gráfico ----

df_beta |>
  ggplot(aes(`Dissimilaridade ambiental`, Composição)) +
  geom_point(size = 5) +
  geom_smooth(method = "glm",
              se = FALSE) +
  ggtext::geom_richtext(data = sts_grafico_beta,
                        aes(`Dissimilaridade ambiental`, Composição, label = sts),
                        label.colour = NA,
                        fill = NA,
                        size = 7.5) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 25),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 19),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "none",
        title = element_text(color = "black", size = 25),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "grafico_pontos_beta_distancia_borda.png",
       height = 10, width = 12)

# Ordenação das espécies ----

especies |>
  dplyr::left_join(ambientais,
                   by = "Unidade Amostral") |>
  ordenaR::order_circle(gradient = "Distância da Borda",
                        species = 2:11,
                        direct = TRUE,
                        range = 15) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "grafico_ordenacao_especies_distancia_borda.png",
       height = 10, width = 12)

# Modelos lineares abundância -----

## Criando o modelo ----

modelos_abund_borda <- function(especie){

  modelo <- glm(df_abund[[especie]] ~ `Distância da Borda`,
                data = df_abund,
                family = poisson(link = "log"))

  assign(paste0("abund_borda_modelo_", especie |> stringr::word(1)),
         modelo,
         envir = globalenv())

}

especie <- especies |>
  dplyr::select(2:4) |>
  names()

especie |> janitor::clean_names()

purrr::map(especie, modelos_abund_borda)

## Pressupostos do modelo ----

pres_abund_borda <- function(modelo, especie){

  stringr::str_glue("Pressupostos para o modelo de {especie}") |>
    crayon::green() |>
    message()

  pressupost_plot <- modelo |> DHARMa::simulateResiduals(plot = TRUE)

  print(pressupost_plot)

}

modelo <- ls(pattern = "abund_borda_modelo_") |>
  mget(envir = globalenv())

modelo

purrr::map2(modelo, especie |> sort(), pres_abund_borda)

## Estatísticas do modelo ----

sts_abund_borda <- function(modelo, especie){

  stringr::str_glue("Estatísticas o modelo de {especie}") |>
    crayon::green() |>
    message()

  smry_plot <- modelo |> summary()

  print(smry_plot)

  assign(paste0("sts_modelo_", especie |> stringr::word(1)),
         smry_plot,
         envir = globalenv())

  stringr::str_glue("pseudo-R² do modelo de {especie}") |>
    crayon::green() |>
    message()

  pseudo_r2 <- abund_borda_modelo_Pristimantis |> performance::r2_mcfadden()

  print(pseudo_r2)

  assign(paste0("pseudor2_", especie |> stringr::word(1)),
         pseudo_r2,
         envir = globalenv())

}

purrr::map2(modelo, especie |> sort(), sts_abund_borda)

## Dataframe das estatísticas do modelo ----

sts_dfs <- function(summary, especie){

  coeficiente <- sts_modelo_Adenomera$coefficients

  df_sts <- tibble::tibble(sts = paste0("β1 ± EP = ",
                                        coeficiente[2, 1] |> round(5),
                                        " ± ",
                                        coeficiente[2, 2] |> round(5),
                                        "<br>z = ",
                                        coeficiente[2, 3] |> round(2)))
}

## Gráfico ----
