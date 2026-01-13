# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(sf)

library(elevatr)

library(terra)

library(DHARMa)

library(performance)

library(ggtext)

# Dados ----

## Abundância de espécies ----

### Importando ----

especies <- readxl::read_xlsx("levantamento_anuros.xlsx")

### Visualizando ----

especies

especies |> dplyr::glimpse()

### Tratando ----

especies %<>%
  dplyr::mutate(`Segmento da parcelas` = `Segmento da parcelas` |>
                  as.character())

especies |> dplyr::glimpse()

## Variáveis ambientais ----

### Importando ----

ambientais <-readxl::read_xlsx("matriz_ambientais.xlsx")

### Visualizando ----

ambientais

ambientais |> dplyr::glimpse()

# Dataframe do modelo -----

## Abundância -----

### Criando a matriz ----

especies_ocup <- especies |>
  dplyr::filter(Epípeto %in% c("ramagii", "hylaedactyla", "hoogmoedi")) |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(`Unidade Amostral`, Espécie, Campanha)) |>
  dplyr::summarise(Abundância = Abundância |> max(),
                   .by = c(`Unidade Amostral`, Espécie)) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Abundância,
                     values_fill = 0)

especies_ocup

### Criando o dataframe ----

df_ocupacao <- especies_ocup |>
  dplyr::left_join(ambientais,
                   by = "Unidade Amostral")

df_ocupacao

df_ocupacao |> glimpse()

df_ocupacao |>
  dplyr::select(c(5, 6, 8:10, 12)) |>
  glimpse()

# Modelos lineares ----

## Pristimantis ramagii ----

### Múltiplos modelos ----

rodando_modelos_pristimantis <- function(id){

  nome <- df_ocupacao[, id] |> names()

  paste0("Criando o modelo de Pristimantis para: ",
         nome) |>
    crayon::green() |>
    message()

  modelo <- glm(`Pristimantis ramagii` ~ .,
                data = df_ocupacao[, c(2, id)],
                family = poisson(link = "log"))

  nome <- df_ocupacao[, id] |>
    names() |>
    stringr::word(1)

  assign(paste0("modelo_pristimantis_", nome),
         modelo,
         envir = globalenv())

  paste0("pressupostos do modelo de Pristimantis para: ",
         nome) |>
    crayon::green() |>
    message()

  avaliacao <- modelo |>
    DHARMa::simulateResiduals(plot = TRUE)

  print(avaliacao)

  r2 <- modelo |>
    performance::r2() |>
    as.numeric() |>
    round(2)

  paste0("pseudo-R²: ",
         r2) |>
    crayon::green() |>
    message()

  resultados <- modelo |>
    summary() %>%
    .$coefficient |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::mutate(rowname = rowname |>
                    stringr::str_remove_all("`"),
                  AIC = modelo |> AIC()) |>
    dplyr::relocate(AIC,
                    .before = `z value`) |>
    dplyr::filter(!rowname |> stringr::str_detect("Intercept")) |>
    dplyr::mutate(`pseudo-R²` = r2)

  assign(paste0("resultados_pristimantis_", nome),
         resultados,
         envir = globalenv())

}

purrr::walk(c(5, 6, 8:10, 12), rodando_modelos_pristimantis)

ls(pattern = "modelo_pristimantis_") |>
  mget(envir = globalenv())

ls(pattern = "resultados_pristimantis_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

### Modelo múltiplo ----

### Mlticolinearidade ----

df_ocupacao[, c(5, 6, 8:10, 12)] |>
  cor(method = "spearman")

#### Criando o modelo ----

glm(`Pristimantis ramagii` ~ .,
    data = df_ocupacao[, c(2, 5, 6, 8:10, 12)],
    family = poisson(link = "log")) |>
  summary()

#### Pressupostos do modelo ----

glm(`Pristimantis ramagii` ~ .,
    data = df_ocupacao[, c(2, 5, 6, 8:10, 12)],
    family = poisson(link = "log")) |>
  DHARMa::simulateResiduals(plot = TRUE)

#### Pseudo-R² ----

glm(`Pristimantis ramagii` ~ .,
    data = df_ocupacao[, c(2, 5, 6, 8:10, 12)],
    family = poisson(link = "log")) |>
  performance::r2_mcfadden()

## Adenomera Hylaedactyla ----

### Múltiplos modelos ----

rodando_modelos_adenomera <- function(id){

  nome <- df_ocupacao[, id] |> names()

  paste0("Criando o modelo de Adenomera para: ",
         nome) |>
    crayon::green() |>
    message()

  modelo <- glm(`Adenomera hylaedactyla` ~ .,
                data = df_ocupacao[, c(3, id)],
                family = poisson(link = "log"))

  nome <- df_ocupacao[, id] |>
    names() |>
    stringr::word(1)

  assign(paste0("modelo_adenomera_", nome),
         modelo,
         envir = globalenv())

  paste0("pressupostos do modelo de Adenomera para: ",
         nome) |>
    crayon::green() |>
    message()

  avaliacao <- modelo |>
    DHARMa::simulateResiduals(plot = TRUE)

  print(avaliacao)

  r2 <- modelo |>
    performance::r2() |>
    as.numeric() |>
    round(2)

  paste0("pseudo-R²: ",
         r2) |>
    crayon::green() |>
    message()

  resultados <- modelo |>
    summary() %>%
    .$coefficient |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::mutate(rowname = rowname |>
                      stringr::str_remove_all("`"),
                    AIC = modelo |> AIC()) |>
    dplyr::relocate(AIC,
                    .before = `z value`) |>
    dplyr::filter(!rowname |> stringr::str_detect("Intercept")) |>
    dplyr::mutate(`pseudo-R²` = r2)

  assign(paste0("resultados_adenomera_", nome),
         resultados,
         envir = globalenv())

}

purrr::walk(c(5, 6, 8, 10, 12), rodando_modelos_adenomera)

ls(pattern = "modelo_adenomera_") |>
  mget(envir = globalenv())

ls(pattern = "resultados_adenomera_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

### Modelo múltiplo ----

#### Criando o modelo ----

glm(`Adenomera hylaedactyla` ~ .,
    data = df_ocupacao[, c(3, 5, 6, 8, 10, 12)],
    family = poisson(link = "log")) |>
  summary()

#### Pressupostos do modelo ----

glm(`Adenomera hylaedactyla` ~ .,
    data = df_ocupacao[, c(3, 5, 6, 8, 10, 12)],
    family = poisson(link = "log")) |>
  DHARMa::simulateResiduals(plot = TRUE)

#### Pseudo-R² ----

glm(`Adenomera hylaedactyla` ~ .,
    data = df_ocupacao[, c(3, 5, 6, 8, 10, 12)],
    family = poisson(link = "log")) |>
  performance::r2_mcfadden()

## Rhinella hoogmoedi ----

rodando_modelos_rhinella <- function(id){

  nome <- df_ocupacao[, id] |> names()

  paste0("Criando o modelo de Rhinella para: ",
         nome) |>
    crayon::green() |>
    message()

  modelo <- glm(`Rhinella hoogmoedi` ~ .,
                data = df_ocupacao[, c(4, id)],
                family = poisson(link = "log"))

  nome <- df_ocupacao[, id] |>
    names() |>
    stringr::word(1)

  assign(paste0("modelo_rhinella_", nome),
         modelo,
         envir = globalenv())

  paste0("pressupostos do modelo de Rhinella para: ",
         nome) |>
    crayon::green() |>
    message()

  avaliacao <- modelo |>
    DHARMa::simulateResiduals(plot = TRUE)

  print(avaliacao)

  r2 <- modelo |>
    performance::r2() |>
    as.numeric() |>
    round(3)

  paste0("pseudo-R²: ",
         r2) |>
    crayon::green() |>
    message()

  resultados <- modelo |>
    summary() %>%
    .$coefficient |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::mutate(rowname = rowname |>
                    stringr::str_remove_all("`"),
                  AIC = modelo |> AIC()) |>
    dplyr::relocate(AIC,
                    .before = `z value`) |>
    dplyr::filter(!rowname |> stringr::str_detect("Intercept")) |>
    dplyr::mutate(`pseudo-R²` = r2)

  assign(paste0("resultados_rhinella_", nome),
         resultados,
         envir = globalenv())

}

purrr::walk(c(5, 6, 8:10, 12), rodando_modelos_rhinella)

ls(pattern = "modelo_rhinella_") |>
  mget(envir = globalenv())

ls(pattern = "resultados_rhinella_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

### Modelo múltiplo ----

#### Criando o modelo ----

glm(`Rhinella hoogmoedi` ~ .,
    data = df_ocupacao[, c(4, 5, 6, 8:10, 12)],
    family = poisson(link = "log")) |>
  summary()

#### Pressupostos do modelo ----

glm(`Rhinella hoogmoedi` ~ .,
    data = df_ocupacao[, c(4, 5, 6, 8, 10, 12)],
    family = poisson(link = "log")) |>
  DHARMa::simulateResiduals(plot = TRUE)

#### Pseudo-R² ----

glm(`Rhinella hoogmoedi` ~ .,
    data = df_ocupacao[, c(4, 5, 6, 8, 10, 12)],
    family = poisson(link = "log")) |>
  performance::r2_mcfadden()

# Esatísticas ----

## Pristimantis ramagii ----

### Múltiplos modelos ----

estatisticas_pristimantis <- ls(pattern = "resultados_pristimantis_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(Estimate = Estimate |> round(3),
                `Std. Error` = `Std. Error` |> round(4),
                AIC = AIC |> round(2),
                `z value` = `z value` |> round(2),
                `Pr(>|z|)` = `Pr(>|z|)` |> round(2),
                `Pr(>|z|)` = dplyr::case_when(`Pr(>|z|)` < 0.01 ~ "< 0.01",
                                              .default = `Pr(>|z|)` |>
                                                as.character()),
                `Valor preditor` = c(0.155, 91.6, 5, 7.5, 350, 25.75),
                `Pristimantis ramagii` = 28,
                estatistica = paste0("β1 ± EP = ",
                                     Estimate,
                                     " ± ",
                                     `Std. Error`,
                                     ", AIC = ",
                                     AIC,
                                     "<br>z = ",
                                     `z value`,
                                     "<sub>6</sub>, p = ",
                                     `Pr(>|z|)`,
                                     ", pseudo-R² = ",
                                     `pseudo-R²`),
                rowname = rowname |> stringr::str_remove_all("`")) |>
  rename("Preditor" = rowname) |>
  dplyr::select(1, 8:10)

estatisticas_pristimantis

### Modelo múltiplo ----

pristimantis_stats <- glm(`Pristimantis ramagii` ~ .,
    data = df_ocupacao[, c(2, 5, 6, 8, 10, 12)],
    family = poisson(link = "log")) |>
  summary() %>%
  .$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::slice_tail(n = 5) |>
  dplyr::mutate(Estimate = Estimate |> round(3),
                `Std. Error` = `Std. Error` |> round(4),
                `z value` = `z value` |> round(2),
                `Pr(>|z|)` = `Pr(>|z|)` |> round(3),
                `Pr(>|z|)` = dplyr::case_when(`Pr(>|z|)` < 0.01 ~ "< 0.01",
                                              .default = `Pr(>|z|)` |>
                                                as.character()),
                `Valor preditor` = c(7.5, 0.155, 5, 350, 91.6),
                `Pristimantis ramagii` = 28,
                estatistica = paste0("β1 ± EP = ",
                                     Estimate,
                                     " ± ",
                                     `Std. Error`,
                                     "<br>z = ",
                                     `z value`,
                                     "<sub>6</sub>, p = ",
                                     `Pr(>|z|)`),
                rowname = rowname |> stringr::str_remove_all("`")) |>
  rename("Preditor" = rowname) |>
  dplyr::select(1, 6:8)

pristimantis_stats

## Adenomera hylaedactyla ----

### Múltiplos modelos ----

estatisticas_adenomera <- ls(pattern = "resultados_adenomera_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(Estimate = Estimate |> round(4),
                `Std. Error` = `Std. Error` |> round(4),
                AIC = AIC |> round(2),
                `z value` = `z value` |> round(2),
                `Pr(>|z|)` = `Pr(>|z|)` |> round(2),
                `Pr(>|z|)` = dplyr::case_when(`Pr(>|z|)` < 0.01 ~ "< 0.01",
                                              .default = `Pr(>|z|)` |>
                                                as.character()),
                `Valor preditor` = c(0.155, 91.6, 5, 7.5, 350),
                `Adenomera hylaedactyla` = 17,
                estatistica = paste0("β1 ± EP = ",
                                     Estimate,
                                     " ± ",
                                     `Std. Error`,
                                     ", AIC = ",
                                     AIC,
                                     "<br>z = ",
                                     `z value`,
                                     "<sub>6</sub>, p = ",
                                     `Pr(>|z|)`,
                                     ", pseudo-R² = ",
                                     `pseudo-R²`),
                rowname = rowname |> stringr::str_remove_all("`")) |>
  rename("Preditor" = rowname) |>
  dplyr::select(1, 8:10)

estatisticas_adenomera

### Modelo múltiplo ----

adenomera_stats <- glm(`Adenomera hylaedactyla` ~ .,
                          data = df_ocupacao[, c(3, 5, 6, 8, 10, 12)],
                          family = poisson(link = "log")) |>
  summary() %>%
  .$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::slice_tail(n = 5) |>
  dplyr::mutate(Estimate = Estimate |> round(3),
                `Std. Error` = `Std. Error` |> round(4),
                `z value` = `z value` |> round(2),
                `Pr(>|z|)` = `Pr(>|z|)` |> round(3),
                `Pr(>|z|)` = dplyr::case_when(`Pr(>|z|)` < 0.01 ~ "< 0.01",
                                              .default = `Pr(>|z|)` |>
                                                as.character()),
                `Valor preditor` = c(7.5, 0.155, 5, 350, 91.6),
                `Adenomera hylaedactyla` = 17,
                estatistica = paste0("β1 ± EP = ",
                                     Estimate,
                                     " ± ",
                                     `Std. Error`,
                                     "<br>z = ",
                                     `z value`,
                                     "<sub>6</sub>, p = ",
                                     `Pr(>|z|)`),
                rowname = rowname |> stringr::str_remove_all("`")) |>
  rename("Preditor" = rowname) |>
  dplyr::select(1, 6:8)

adenomera_stats

## Rhinella hoogmoedi ----

### Múltiplos modelos ----

estatisticas_rhinella <- ls(pattern = "resultados_rhinella_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(Estimate = Estimate |> round(4),
                `Std. Error` = `Std. Error` |> round(4),
                AIC = AIC |> round(2),
                `z value` = `z value` |> round(2),
                `Pr(>|z|)` = `Pr(>|z|)` |> round(2),
                `Pr(>|z|)` = dplyr::case_when(`Pr(>|z|)` < 0.01 ~ "< 0.01",
                                              .default = `Pr(>|z|)` |>
                                                as.character()),
                `Valor preditor` = c(0.155, 91.6, 5, 7.5, 350, 25.75),
                `Rhinella hoogmoedi` = 10.5,
                estatistica = paste0("β1 ± EP = ",
                                     Estimate,
                                     " ± ",
                                     `Std. Error`,
                                     ", AIC = ",
                                     AIC,
                                     "<br>z = ",
                                     `z value`,
                                     "<sub>6</sub>, p = ",
                                     `Pr(>|z|)`,
                                     ", pseudo-R² = ",
                                     `pseudo-R²`),
                rowname = rowname |> stringr::str_remove_all("`")) |>
  rename("Preditor" = rowname) |>
  dplyr::select(1, 8:10)

estatisticas_rhinella

### Modelo múltiplo ----

rhinella_stats <- glm(`Rhinella hoogmoedi` ~ .,
                       data = df_ocupacao[, c(4, 5, 6, 8, 10, 12)],
                       family = poisson(link = "log")) |>
  summary() %>%
  .$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::slice_tail(n = 5) |>
  dplyr::mutate(Estimate = Estimate |> round(3),
                `Std. Error` = `Std. Error` |> round(4),
                `z value` = `z value` |> round(2),
                `Pr(>|z|)` = `Pr(>|z|)` |> round(3),
                `Pr(>|z|)` = dplyr::case_when(`Pr(>|z|)` < 0.01 ~ "< 0.01",
                                              .default = `Pr(>|z|)` |>
                                                as.character()),
                `Valor preditor` = c(7.5, 0.155, 5, 350, 91.6),
                `Rhinella hoogmoedi` = 10.5,
                estatistica = paste0("β1 ± EP = ",
                                     Estimate,
                                     " ± ",
                                     `Std. Error`,
                                     "<br>z = ",
                                     `z value`,
                                     "<sub>6</sub>, p = ",
                                     `Pr(>|z|)`),
                rowname = rowname |> stringr::str_remove_all("`")) |>
  rename("Preditor" = rowname) |>
  dplyr::select(1, 6:8)

rhinella_stats

# Gráficos ----

## Pstimantis ramagii ----

df_ocupacao |>
  tidyr::pivot_longer(cols = c(5, 6, 8:10, 12),
                      names_to = "Preditor",
                      values_to = "Valor preditor") |>
  ggplot(aes(`Valor preditor`, `Pristimantis ramagii`,
             fill = Preditor, color = Preditor)) +
  geom_point(shape = 21, color = "black", stroke = 1,
             size = 3.5, show.legend = FALSE) +
  ggtext::geom_richtext(data = estatisticas_pristimantis,
                        aes(label = estatistica),
                        color = "black",
                        fontface = "bold",
                        label.colour = "transparent",
                        fill = "transparent",
                        size = 4.5) +
  facet_wrap(~Preditor, scales = "free_x") +
  geom_smooth(data = . %>%
                dplyr::filter(Preditor %in% c("Área das poças",
                                              "Distância dos corpos hídricos",
                                              "Temperatura")),
              method = "glm", show.legend = FALSE, se = FALSE) +
  labs(y = "Abundância") +
  scale_fill_manual(values = c("green2",
                               "gold",
                               "orange2",
                               "royalblue",
                               "skyblue",
                               "orangered")) +
  scale_color_manual(values = c("blue",
                                "skyblue4",
                                "darkorange")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 15),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "none",
        title = element_text(color = "black", size = 15),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10,
                 width = 12)

ggsave(filename = "modelo_abundancia_pristimantis_multiplo.png",
       height = 10, width = 12)

## Adenomera hylaedactyla ----

df_ocupacao |>
  tidyr::pivot_longer(cols = c(5, 6, 8, 10, 12),
                      names_to = "Preditor",
                      values_to = "Valor preditor") |>
  ggplot(aes(`Valor preditor`, `Adenomera hylaedactyla`,
             fill = Preditor, color = Preditor)) +
  geom_point(shape = 21, color = "black", stroke = 1,
             size = 3.5, show.legend = FALSE) +
  ggtext::geom_richtext(data = estatisticas_adenomera,
                        aes(label = estatistica),
                        color = "black",
                        fontface = "bold",
                        label.colour = "transparent",
                        fill = "transparent",
                        size = 4.5) +
  facet_wrap(~Preditor, scales = "free_x") +
  labs(y = "Abundância") +
  scale_fill_manual(values = c("green2",
                               "gold",
                               "orange2",
                               "royalblue",
                               "skyblue")) +
  scale_color_manual(values = c("green4",
                               "skyblue4")) +
  scale_y_continuous(limits = c(4, 18)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 15),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "none",
        title = element_text(color = "black", size = 15),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10,
                 width = 12)

ggsave(filename = "modelo_abundancia_adenomera_multiplo.png",
       height = 10, width = 12)

## Rhinella hoogmoedi ----

df_ocupacao |>
  tidyr::pivot_longer(cols = c(5, 6, 8:10, 12),
                      names_to = "Preditor",
                      values_to = "Valor preditor") |>
  ggplot(aes(`Valor preditor`, `Rhinella hoogmoedi`,
             fill = Preditor, color = Preditor)) +
  geom_point(shape = 21, color = "black", stroke = 1,
             size = 3.5, show.legend = FALSE) +
  ggtext::geom_richtext(data = estatisticas_rhinella,
                        aes(label = estatistica),
                        color = "black",
                        fontface = "bold",
                        label.colour = "transparent",
                        fill = "transparent",
                        size = 4.5) +
  facet_wrap(~Preditor, scales = "free_x") +
  geom_smooth(data = . %>%
                dplyr::filter(Preditor %in% c("Abertura do dossel",
                                             "Altitude")),
              method = "glm", family = poisson, show.legend = FALSE, se = FALSE) +
  scale_fill_manual(values = c("green2",
                               "gold",
                               "orange2",
                               "royalblue",
                               "skyblue",
                               "orangered")) +
  scale_color_manual(values = c("darkgreen",
                                "gold4")) +
  labs(y = "Abundância") +
  scale_y_continuous(limits = c(1, 12)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 15),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "none",
        title = element_text(color = "black", size = 15),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10,
                 width = 12)

ggsave(filename = "modelo_abundancia_rhinella_multiplo.png",
       height = 10, width = 12)
