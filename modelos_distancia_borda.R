# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(performance)

library(ggtext)

library(ggview)

library(glmmTMB)

library(ordenaR)

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

## Calculando a diversidade ---

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

## Estatísticas o modelo ----

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
                                df_beta)

## Pressupostos do modelo ----

modelo_beta |> performance::check_model(check = c("homogeneity",
                                                  "qq",
                                                  "normality"))

## Estatísticas o modelo ----

sts_modelo_beta <- modelo_beta |> summary()

sts_modelo_beta

## Dataframe das estatísticas do modelo ----

sts_grafico_beta <- tibble::tibble(sts = paste0("β1 ± EP = ",
                                                sts_modelo_beta$coefficients$cond[2, 1] |> round(5),
                                                " ± ",
                                                sts_modelo_beta$coefficients$cond[2, 2] |> round(4),
                                                "<br>z = ",
                                                sts_modelo_beta$coefficients$cond[2, 3] |> round(2),
                                                ", p = ",
                                                sts_modelo_beta$coefficients$cond[2, 4] |> round(3)),
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
