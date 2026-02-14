# Pacotes -----

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

## Abundância de espécies -----

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
                   by = "Unidade Amostral") |>
  dplyr::rename("Canopy openness" = 6,
                "Leaf-litter depth" = 8,
                "Temperature" = 9,
                "Hydric stream distance" = 10,
                "Edge distance" = 11,
                "Elevation" = 12)

df_ocupacao

df_ocupacao |> glimpse()

df_ocupacao |>
  dplyr::select(c(6, 8, 10:12)) |>
  glimpse()

# Modelos lineares ----

## Pristimantis ramagii ----

rodando_modelos_pristimantis <- function(id){

  nome <- df_ocupacao[, id] |> names()

  paste0("Criando o modelo de Pristimantis para: ",
         nome) |>
    crayon::green() |>
    message()

  modelo <- glm(`Pristimantis ramagii` ~ .,
                data = df_ocupacao[, c(2, id, 9)],
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
    performance::r2_mcfadden() |>
    as.numeric() |>
    round(2)

  paste0("pseudo-R²: ",
         r2) |>
    crayon::green() |>
    message()

  nome <- df_ocupacao[, id] |> names()

  resultados <- modelo |>
    summary() %>%
    .$coefficient |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::mutate(rowname = rowname |>
                    stringr::str_remove_all("`")) |>
    dplyr::filter(!rowname |> stringr::str_detect("Intercept")) |>
    dplyr::mutate(`pseudo-R²` = r2[2],
                  Modelo = nome,
                  `Pr(>|z|)` = dplyr::case_when(`Pr(>|z|)` < 0.01 ~ "< 0.01",
                                                .default = paste0("= ",
                                                                  `Pr(>|z|)` |>
                                                                    round(2)))) |>
    dplyr::relocate(Modelo, .before = rowname)

  assign(paste0("resultados_pristimantis_", nome),
         resultados,
         envir = globalenv())

}

purrr::walk(c(6, 8, 10:12), rodando_modelos_pristimantis)

ls(pattern = "modelo_pristimantis_") |>
  mget(envir = globalenv())

ls(pattern = "resultados_pristimantis_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

ls(pattern = "resultados_pristimantis_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::filter(!rowname == "Temperature")

## Adenomera Hylaedactyla ----

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
    performance::r2_mcfadden() |>
    as.numeric() |>
    round(2)

  paste0("pseudo-R²: ",
         r2) |>
    crayon::green() |>
    message()

  nome <- df_ocupacao[, id] |> names()

  resultados <- modelo |>
    summary() %>%
    .$coefficient |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::mutate(rowname = rowname |>
                      stringr::str_remove_all("`")) |>
    dplyr::filter(!rowname |> stringr::str_detect("Intercept")) |>
    dplyr::mutate(`pseudo-R²` = r2[2],
                  Modelo = nome,
                  `Pr(>|z|)` = dplyr::case_when(`Pr(>|z|)` < 0.01 ~ "< 0.01",
                                                .default = paste0("= ",
                                                                  `Pr(>|z|)` |>
                                                                    round(2)))) |>
    dplyr::relocate(Modelo, .before = rowname)

  assign(paste0("resultados_adenomera_", nome),
         resultados,
         envir = globalenv())

}

purrr::walk(c(6, 8, 10:12), rodando_modelos_adenomera)

ls(pattern = "modelo_adenomera_") |>
  mget(envir = globalenv())

ls(pattern = "resultados_adenomera_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

ls(pattern = "resultados_adenomera_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

## Rhinella hoogmoedi ----

rodando_modelos_rhinella <- function(id){

  nome <- df_ocupacao[, id] |> names()

  paste0("Criando o modelo de Rhinella para: ",
         nome) |>
    crayon::green() |>
    message()

  modelo <- glm(`Rhinella hoogmoedi` ~ .,
                data = df_ocupacao[, c(4, id, 9)],
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
    performance::r2_mcfadden() |>
    as.numeric() |>
    round(3)

  paste0("pseudo-R²: ",
         r2) |>
    crayon::green() |>
    message()

  nome <- df_ocupacao[, id] |> names()

  resultados <- modelo |>
    summary() %>%
    .$coefficient |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::mutate(rowname = rowname |>
                    stringr::str_remove_all("`")) |>
    dplyr::filter(!rowname |> stringr::str_detect("Intercept")) |>
    dplyr::mutate(`pseudo-R²` = r2[2],
                  Modelo = nome,
                  `Pr(>|z|)` = dplyr::case_when(`Pr(>|z|)` < 0.01 ~ "< 0.01",
                                                .default = paste0("= ",
                                                                  `Pr(>|z|)` |>
                                                                    round(2)))) |>
    dplyr::relocate(Modelo, .before = rowname)

  assign(paste0("resultados_rhinella_", nome),
         resultados,
         envir = globalenv())

}

purrr::walk(c(6, 8, 10:12), rodando_modelos_rhinella)

ls(pattern = "modelo_rhinella_") |>
  mget(envir = globalenv())

ls(pattern = "resultados_rhinella_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

ls(pattern = "resultados_rhinella_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::filter(!rowname == "Temperature")

# Estatísticas ----

## Pristimantis ramagii ----

sts_pristimantis <- ls(pattern = "resultados_pristimantis_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(Estimate_temp = Estimate |> dplyr::lead(),
                `Std. Error temp` = `Std. Error` |> dplyr::lead(),
                z_temp = `z value` |> dplyr::lead() |> round(2),
                p_temp = `Pr(>|z|)` |> dplyr::lead()) |>
  dplyr::filter(!rowname == "Temperature") |>
  dplyr::mutate(Estimate = Estimate |> round(3),
                Estimate_temp = Estimate_temp |> round(3),
                `Std. Error` = `Std. Error` |> round(4),
                `Std. Error temp` = `Std. Error temp` |> round(4),
                `z value` = `z value` |> round(2),
                `Valor preditor` = c(0.155, 450, 91.6, 300, 5),
                `Pristimantis ramagii` = 29,
                estatistica = paste0("β1 ± EP<sub>",
                                     rowname,
                                     "</sub> = ",
                                     Estimate,
                                     " ± ",
                                     `Std. Error`,
                                     "<br>z = ",
                                     `z value`,
                                     "<sub>6</sub>, p ",
                                     `Pr(>|z|)`,
                                     "<br>β1 ± EP<sub>temperature</sub> = ",
                                     Estimate_temp,
                                     " ± ",
                                     `Std. Error temp`,
                                     "<br>z = ",
                                     z_temp,
                                     "<sub>6</sub>, p ",
                                     p_temp,
                                     ", pseudo-R² = ",
                                     `pseudo-R²`),
                rowname = rowname |> stringr::str_remove_all("`"),
                rowname = paste0(rowname, " + Temperature")) |>
  rename("Preditor" = rowname) |>
  dplyr::mutate(Significante = dplyr::case_when(`Pr(>|z|)` == "< 0.01" ~ "Sim",
                                                `Pr(>|z|)` |>
                                                  readr::parse_number() < 0.05 ~ "Sim",
                                                .default = "Não"),
                Preditor = Preditor |>
                  forcats::fct_relevel(c("Leaf-litter depth + Temperature",
                                         "Canopy openness + Temperature",
                                         "Edge distance + Temperature",
                                         "Elevation + Temperature",
                                         "Water area + Temperature"))) |>
  dplyr::select(2, 12:15)

sts_pristimantis

## Adenomera hylaedactyla ----

sts_adenomera <- ls(pattern = "resultados_adenomera_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(Estimate = Estimate |> round(3),
                `Std. Error` = `Std. Error` |> round(4),
                `z value` = `z value` |> round(2),
                `Valor preditor` = c(0.155, 450, 91.6, 300, 5),
                `Adenomera hylaedactyla` = 17,
                estatistica = paste0("β1 ± EP<sub>",
                                     rowname,
                                     "</sub> = ",
                                     Estimate,
                                     " ± ",
                                     `Std. Error`,
                                     "<br>z = ",
                                     `z value`,
                                     "<sub>6</sub>, p ",
                                     `Pr(>|z|)`,
                                     ", pseudo-R² = ",
                                     `pseudo-R²`),
                rowname = rowname |> stringr::str_remove_all("`")) |>
  rename("Preditor" = rowname) |>
  dplyr::mutate(Significante = dplyr::case_when(`Pr(>|z|)` == "< 0.01" ~ "Sim",
                                                `Pr(>|z|)` |>
                                                  as.numeric() < 0.05 ~ "Sim",
                                                .default = "Não")) |>
  dplyr::select(2, 8:11)

sts_adenomera

## Rhinella hoogmoedi ----

sts_rhinella <- ls(pattern = "resultados_rhinella_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(Estimate_temp = Estimate |> dplyr::lead(),
                `Std. Error temp` = `Std. Error` |> dplyr::lead(),
                z_temp = `z value` |> dplyr::lead() |> round(2),
                p_temp = `Pr(>|z|)` |> dplyr::lead()) |>
  dplyr::filter(!rowname == "Temperature") |>
  dplyr::mutate(Estimate = Estimate |> round(3),
                Estimate_temp = Estimate_temp |> round(3),
                `Std. Error` = `Std. Error` |> round(4),
                `Std. Error temp` = `Std. Error temp` |> round(4),
                `z value` = `z value` |> round(2),
                `Valor preditor` = c(0.155, 450, 91.6, 300, 5),
                `Rhinella hoogmoedi` = 11,
                estatistica = paste0("β1 ± EP<sub>",
                                     rowname,
                                     "</sub> = ",
                                     Estimate,
                                     " ± ",
                                     `Std. Error`,
                                     "<br>z = ",
                                     `z value`,
                                     "<sub>6</sub>, p ",
                                     `Pr(>|z|)`,
                                     "<br>β1 ± EP<sub>temperature</sub> = ",
                                     Estimate_temp,
                                     " ± ",
                                     `Std. Error temp`,
                                     "<br>z = ",
                                     z_temp,
                                     "<sub>6</sub>, p ",
                                     p_temp,
                                     ", pseudo-R² = ",
                                     `pseudo-R²`),
                rowname = rowname |> stringr::str_remove_all("`"),
                rowname = paste0(rowname, " + Temperature")) |>
  rename("Preditor" = rowname) |>
  dplyr::mutate(Significante = dplyr::case_when(`Pr(>|z|)` == "< 0.01" ~ "Sim",
                                                `Pr(>|z|)` |>
                                                  readr::parse_number() < 0.05 ~ "Sim",
                                                .default = "Não")) |>
  dplyr::select(2, 12:15)

sts_rhinella

# Gráficos ----

## Pstimantis ramagii ----

df_ocupacao |>
  tidyr::pivot_longer(cols = c(6, 8, 10:12),
                      names_to = "Preditor",
                      values_to = "Valor preditor") |>
  dplyr::mutate(Preditor = paste0(Preditor, " + Temperature"),
                Preditor = Preditor |>
                  forcats::fct_relevel(c("Leaf-litter depth + Temperature",
                                         "Canopy openness + Temperature",
                                         "Edge distance + Temperature",
                                         "Elevation + Temperature",
                                         "Water area + Temperature"))) |>
  dplyr::left_join(sts_pristimantis |>
                     dplyr::select(1, 5),
                   by = "Preditor") |>
  ggplot(aes(`Valor preditor`, `Pristimantis ramagii`)) +
  geom_point(color = "black",
             size = 3.5) +
  ggtext::geom_richtext(data = sts_pristimantis,
                        aes(label = estatistica),
                        color = "black",
                        fontface = "bold",
                        label.colour = "transparent",
                        fill = "transparent",
                        size = 4) +
  facet_wrap(~Preditor, scales = "free_x") +
  geom_smooth(data = . %>%
                dplyr::filter(Significante == "Sim"),
              method = "glm", show.legend = FALSE, se = FALSE) +
  labs(x = "Predictor value",
       y = "<i>Pristimantis ramagii</i> abundance") +
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
  ggview::canvas(height = 10,
                 width = 12)

ggsave(filename = "modelo_abundancia_pristimantis_multiplo.png",
       height = 10, width = 12)

## Adenomera hylaedactyla ----

df_ocupacao |>
  tidyr::pivot_longer(cols = c(6, 8, 10:12),
                      names_to = "Preditor",
                      values_to = "Valor preditor") |>
  dplyr::mutate(Preditor = Preditor |>
                  forcats::fct_relevel(c("Leaf-litter depth",
                                         "Canopy openness",
                                         "Edge distance",
                                         "Elevation",
                                         "Water area"))) |>
  dplyr::left_join(sts_adenomera |>
                     dplyr::select(1, 5),
                   by = "Preditor") |>
  ggplot(aes(`Valor preditor`, `Adenomera hylaedactyla`)) +
  geom_point(color = "black",
             size = 3.5) +
  ggtext::geom_richtext(data = sts_adenomera,
                        aes(label = estatistica),
                        color = "black",
                        fontface = "bold",
                        label.colour = "transparent",
                        fill = "transparent",
                        size = 4) +
  facet_wrap(~Preditor, scales = "free_x") +
  geom_smooth(data = . %>%
                dplyr::filter(Significante == "Sim"),
              method = "glm", show.legend = FALSE, se = FALSE) +
  labs(x = "Predictor value",
       y = "<i>Adenomera</i> aff. <i>hylaedactyla</i> abundance") +
  scale_y_continuous(limits = c(5, 17.5)) +
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
  ggview::canvas(height = 10,
                 width = 12)

ggsave(filename = "modelo_abundancia_adenomera_multiplo.png",
       height = 10, width = 12)

## Rhinella hoogmoedi ----

df_ocupacao |>
  tidyr::pivot_longer(cols = c(6, 8, 10:12),
                      names_to = "Preditor",
                      values_to = "Valor preditor") |>
  dplyr::mutate(Preditor = paste0(Preditor, " + Temperature"),
                Preditor = Preditor |>
                  forcats::fct_relevel(c("Leaf-litter depth + Temperature",
                                         "Canopy openness + Temperature",
                                         "Edge distance + Temperature",
                                         "Elevation + Temperature",
                                         "Water area + Temperature"))) |>
  dplyr::left_join(sts_rhinella |>
                     dplyr::select(1, 5),
                   by = "Preditor") |>
  ggplot(aes(`Valor preditor`, `Rhinella hoogmoedi`)) +
  geom_point(color = "black",
             size = 3.5) +
  ggtext::geom_richtext(data = sts_rhinella,
                        aes(label = estatistica),
                        color = "black",
                        fontface = "bold",
                        label.colour = "transparent",
                        fill = "transparent",
                        size = 4) +
  facet_wrap(~Preditor, scales = "free_x") +
  geom_smooth(data = . %>%
                dplyr::filter(Significante == "Sim"),
              method = "glm", family = poisson, show.legend = FALSE, se = FALSE) +
  labs(x = "Predictor value",
       y = "<i>Rhinella hoogmoedi</i> abundance") +
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
  ggview::canvas(height = 10,
                 width = 12)

ggsave(filename = "modelo_abundancia_rhinella_multiplo.png",
       height = 10, width = 12)
