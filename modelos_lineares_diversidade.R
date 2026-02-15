# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(performance)

library(ggtext)

library(ggview)

library(DHARMa)

library(glmmTMB)

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

# Dataframe do modelo -----

## Riqueza e diversidade alfa -----

### ìndices de diversidade ----

div_alfa <- especies |>
  tibble::column_to_rownames(var = "Unidade Amostral") |>
  vegan::renyi(scales = 1, hill = TRUE) |>
  dplyr::as_data_frame() |>
  dplyr::mutate(`Unidade Amostral` = especies$`Unidade Amostral`) |>
  dplyr::relocate(value, .after = `Unidade Amostral`) |>
  dplyr::rename("Q = 1" = value)

div_alfa

### Criando o dataframe ----

df_alfa <- div_alfa |>
  dplyr::left_join(ambientais,
                   by = "Unidade Amostral") |>
  dplyr::rename("Canopy openness" = `Abertura do dossel`,
                "Edge distance" = `Distância da Borda`,
                "Elevation" = Altitude,
                "Hydric stream distance" = `Distância dos corpos hídricos`,
                "Leaf-litter depth" = `Altura da serrapilheira`)

df_alfa

df_alfa |> dplyr::glimpse()

## Diversidade beta -----

### Matriz de composição ----

matriz_comp <- especies |>
  tibble::column_to_rownames(var = "Unidade Amostral") |>
  vegan::decostand(method = "hellinger") |>
  vegan::vegdist() |>
  as.numeric()

matriz_comp

### Matrizes ambientais ----

matriz_dossel <- ambientais |>
  dplyr::select(`Abertura do dossel`) |>
  vegan::vegdist(method = "euclidean") |>
  as.numeric()

matriz_dossel

matriz_serrapilheira <- ambientais |>
  dplyr::select(`Altura da serrapilheira`) |>
  vegan::vegdist(method = "euclidean") |>
  as.numeric()

matriz_serrapilheira

matriz_distancia <- ambientais |>
  dplyr::select(`Distância dos corpos hídricos`) |>
  vegan::vegdist(method = "euclidean") |>
  as.numeric()

matriz_distancia

matriz_temperatura <- ambientais |>
  dplyr::select(Temperatura) |>
  vegan::vegdist(method = "euclidean") |>
  as.numeric()

matriz_temperatura

matriz_borda <- ambientais |>
  dplyr::select(`Distância da Borda`) |>
  vegan::vegdist(method = "euclidean") |>
  as.numeric()

matriz_borda

matriz_altitude <- ambientais |>
  dplyr::select(Altitude) |>
  vegan::vegdist(method = "euclidean") |>
  as.numeric()

matriz_altitude

### Criando o dataframe ----

df_beta <- tibble::tibble(Composition = matriz_comp,
                          `Canopy openness` = matriz_dossel,
                          `Leaf-litter depth` = matriz_serrapilheira,
                          `Hydric stream distance` = matriz_distancia,
                          `Edge distance` = matriz_borda,
                          `Elevation` = matriz_altitude)

df_beta

df_beta |> dplyr::glimpse()

# Modelos lineares ----

## Diversidade alfa ----

### Múltiplos modelos ----

modelos_diversidade <- function(id){

  id <- as.integer(id)

  nome <- df_alfa[id] |> names()

  paste0("Criando o modelo para: ",
         nome) |>
    crayon::green() |>
    message()

  modelo <- lm(`Q = 1` ~ .,
               data = df_alfa |>
                 dplyr::select(2, id))

  nome <- df_alfa[, id] |>
    names() |>
    stringr::word(1)

  assign(paste0("modelo_alfa_", nome),
         modelo |> summary(),
         envir = globalenv())

  paste0("pressupostos do modelo de: ",
         nome) |>
    crayon::green() |>
    message()

  modelo |>
    performance::check_heteroscedasticity() |>
    print()

  modelo |>
    performance::check_normality() |>
    print()

  modelo |>
    performance::check_model(check = c("vif",
                                       "qq",
                                       "normality",
                                       "homogeneity")) |>
    print()

  resultados <- modelo |>
    summary() %>%
    .$coefficient |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::mutate(rowname = rowname |>
                    stringr::str_remove_all("`")) |>
    dplyr::filter(!rowname |> stringr::str_detect("Intercept"))

  summary <- modelo |>
    summary()

  resultados_summary <- tibble::tibble(`F` = summary$fstatistic[1] |> round(2),
                                       df1 = summary$fstatistic[2],
                                       df2 = summary$fstatistic[3],
                                       `p global` = pf(q = summary$fstatistic[1],
                                              df1 = summary$fstatistic[2],
                                              df2 = summary$fstatistic[3],
                                              lower.tail = FALSE) |>
                                         round(2),
                                       `R² ajustado` = summary$adj.r.squared |>
                                         round(2))

  resultados <- resultados |>
    dplyr::bind_cols(resultados_summary)

  assign(paste0("resultados_alfa_", nome),
         resultados,
         envir = globalenv())

}

purrr::walk(c(4, 6, 8:10), modelos_diversidade)

ls(pattern = "modelo_alfa_") |>
  mget(envir = globalenv())

ls(pattern = "resultados_alfa_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

### Dataframe de estatísticas usadas no gráfico ----

#### Valor medano das variáveis ----

medianas_alfa <- df_alfa |>
  dplyr::select(4, 6, 8:10) |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "Preditor",
                      values_to = "Valor preditor") |>
  dplyr::summarise(`Valor Preditor` = `Valor preditor` |> range() |> mean(),
                   .by = Preditor) |>
  dplyr::mutate(Preditor = Preditor |>
                  forcats::fct_relevel(c("Leaf-litter depth",
                                         "Canopy openness",
                                         "Edge distance",
                                         "Elevation",
                                         "Hydric stream distance")))

medianas_alfa

#### Dataframe ----

df_q1_estatisticas <- ls(pattern = "resultados_alfa_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::rename("Preditor" = 1) |>
  dplyr::mutate(Estimate = Estimate |> round(4),
                `Std. Error` = `Std. Error` |> round(4),
                `t value` = `t value` |> round(3),
                `Pr(>|t|)` = `Pr(>|t|)` |> round(2)) |>
  dplyr::relocate(Preditor, .before = Estimate) |>
  dplyr::filter(!Preditor |> stringr::str_detect("\\(")) |>
  dplyr::rename("β1" = Estimate,
                "SE" = `Std. Error`,
                "t" = `t value`,
                "p" = `Pr(>|t|)`) |>
  tidyr::unite(β1:SE,
               sep = " ± ",
               col = "β1 ± SE") |>
  dplyr::mutate(`Q = 1` = 4.3,
                estatistica = paste0("β1 ± SE = ",
                                     `β1 ± SE`,
                                     "<br>t = ",
                                     t,
                                     ", p = ",
                                     p,
                                     "<br>F<sub>",
                                     df1,
                                     ", ",
                                     df2,
                                     "</sub> = ",
                                     `F`,
                                     ", p = ",
                                     `p global`,
                                     ", R² aju. = ",
                                     `R² ajustado`),
                Preditor = Preditor |>
                  forcats::fct_relevel(c("Leaf-litter depth",
                                         "Canopy openness",
                                         "Edge distance",
                                         "Elevation",
                                         "Hydric stream distance")),
                significante = dplyr::case_when(`p global` < 0.05 ~ "Sim",
                                               .default = "Não")) |>
  dplyr::select(-c(2:9)) |>
  dplyr::left_join(medianas_alfa,
                   by = "Preditor")

df_q1_estatisticas

#### Estatísticas críticas ----

q_t <- qt(p = 0.05, df = 10, lower.tail = FALSE) |> round(2)

q_t

q_f <- qf(p = 0.05, df1 = 1, df2 = 9, lower.tail = FALSE) |> round(2)

q_f

#### Gráfico -----

df_alfa |>
  tidyr::pivot_longer(cols = c(4, 6, 8:10),
                      names_to = "Preditor",
                      values_to = "Valor Preditor") |>
  dplyr::mutate(Preditor = Preditor |>
                  forcats::fct_relevel(c("Leaf-litter depth",
                                         "Canopy openness",
                                         "Edge distance",
                                         "Elevation",
                                         "Hydric stream distance"))) |>
  dplyr::left_join(df_q1_estatisticas |>
                     dplyr::select(1, 4),
                   by = "Preditor") |>
  ggplot(aes(`Valor Preditor`,`Q = 1`)) +
  geom_point(color = "black",
             size = 3.5,
             stroke = 1) +
  facet_wrap(~Preditor, scales = "free_x") +
  ggtext::geom_richtext(data = df_q1_estatisticas,
                        aes(label = estatistica),
                        color = "black",
                        fontface = "bold",
                        label.colour = "transparent",
                        fill = "transparent",
                        size = 4.75) +
  geom_smooth(data = . %>%
                dplyr::filter(significante == "Sim"),
              method = "lm",
              se = FALSE) +
  labs(title = paste0("t-critic = ", q_t, ", F-critic = ", q_f),
       x = "Predictor value",
       y = "Diversity (Q = 1)") +
  scale_y_continuous(limits = c(2.5, 4.45)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        axis.title.y = ggtext::element_markdown(color = "black", size = 15),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 15),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "none",
        title = element_text(color = "black", size = 15),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "grafico_pontos_q1.png", height = 10, width = 12)

## Diversidade beta -----

### Multicolinearidade ----

df_beta |>
  dplyr::select(2:6) |>
  cor(method = "spearman")

### Criando o modelo ----

modelo_beta <- glmmTMB::glmmTMB(Composition ~ `Leaf-litter depth` +
                                  `Canopy openness` +
                                  `Edge distance` +
                                  `Elevation` +
                                  `Hydric stream distance`,
                                data = df_beta,
                                family = glmmTMB::beta_family())

### Pressupostos do modelo ----

modelo_beta |>
  DHARMa::simulateResiduals(plot = TRUE)

### Avaliando o modelo ----

modelo_beta |>
  summary()

### Pseudo-R² ----

r2_beta <- modelo_beta |> performance::r2_ferrari() |>
  as.numeric() |>
  round(2)

r2_beta

### Dataframe de estatísticas usadas no gráfico ----

#### Valor mediano das variáveis ----

medianas_beta <- df_beta |>
  dplyr::select(2:6) |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "Preditor",
                      values_to = "Valor preditor") |>
  dplyr::mutate(Preditor = Preditor |>
                  forcats::fct_relevel(c("Leaf-litter depth",
                                         "Canopy openness",
                                         "Edge distance",
                                         "Elevation",
                                         "Hydric stream distance"))) |>
  dplyr::summarise(`Valor Preditor` = `Valor preditor` |> range() |> mean(),
                   .by = Preditor)

medianas_beta

#### Dataframe da tabelas ----

df_sts <- modelo_beta |>
  summary() %>%
  .$coefficients  %>%
  .$cond

nomes_var <- df_sts |> rownames()

nomes_var

df_sts_trat <- df_sts |>
  tibble::as_tibble() |>
  dplyr::mutate(Preditor = nomes_var |>
                  stringr::str_remove_all("`") |>
                  stringr::str_replace("hidrico", "hídrico"),
                Estimate = Estimate |> round(4),
                `Std. Error` = `Std. Error` |> round(4),
                `z value` = `z value` |> round(3),
                `Pr(>|z|)` = `Pr(>|z|)` |> round(5),
                DF = 51) |>
  dplyr::relocate(Preditor, .before = Estimate) |>
  dplyr::filter(!Preditor |> stringr::str_detect("\\(")) |>
  dplyr::rename("β1" = Estimate,
                "EP" = `Std. Error`,
                "z" = `z value`,
                "p" = `Pr(>|z|)`) |>
  tidyr::unite(β1:EP,
               sep = " ± ",
               col = "β1 ± EP") |>
  dplyr::relocate(DF, .before = p) |>
  dplyr::mutate(Composition = 0.52,
                DF = 51,
                estatistica = paste0("β1 ± EP = ",
                                     `β1 ± EP`,
                                     "<br>z = ",
                                     z,
                                     "<sub>",
                                     DF,
                                     "</sub>, p = ",
                                     p |> round(3)),
                significante = dplyr::case_when(p < 0.05 ~ "Sim",
                                                .default = "Não"),
                Preditor = Preditor |>
                  forcats::fct_relevel(c("Leaf-litter depth",
                                         "Canopy openness",
                                         "Edge distance",
                                         "Elevation",
                                         "Hydric stream distance"))) |>
  dplyr::select(-c(2:5)) |>
  dplyr::left_join(medianas_beta,
                   by = "Preditor")

df_sts_trat

#### Gráfico ----

df_beta |>
  tidyr::pivot_longer(cols = 2:6,
                      names_to = "Preditor",
                      values_to = "Valor Preditor") |>
  dplyr::left_join(df_sts_trat |>
                     dplyr::select(1, 4),
                   by = "Preditor")  |>
  dplyr::mutate(Preditor = Preditor |>
                  forcats::fct_relevel(c("Leaf-litter depth",
                                         "Canopy openness",
                                         "Edge distance",
                                         "Elevation",
                                         "Hydric stream distance"))) |>
  ggplot(aes(`Valor Preditor`, Composition)) +
  geom_point(color = "black",
             size = 3.5,
             stroke = 1) +
  geom_smooth(data = . %>%
                dplyr::filter(significante == "Sim"),
              method = "lm",
              se = FALSE) +
  facet_wrap(~Preditor, scales = "free_x") +
  ggtext::geom_richtext(data = df_sts_trat,
                        aes(label = estatistica),
                        color = "black",
                        fontface = "bold",
                        label.colour = "transparent",
                        fill = "transparent",
                        size = 5) +
  scale_y_continuous(limits = c(0.1, 0.5325)) +
  labs(x = "Predictor distance",
       y = "Composition distance",
       title = paste0("z-critic = 1.96, adjusted pseudo-R² = ", r2_beta)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        axis.title.y = ggtext::element_markdown(color = "black", size = 15),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 15),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "none",
        title = element_text(color = "black", size = 15),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "grafico_pontos_beta.png", height = 10, width = 12)
