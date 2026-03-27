# Pacotes -----

library(readxl)

library(tidyverse)

library(magrittr)

library(sf)

library(elevatr)

library(terra)

library(DHARMa)

library(performance)

library(flextable)

library(ggeffects)

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
    dplyr::mutate(Species = "Pristimantis ramagii",
                  rowname = rowname |>
                    stringr::str_remove_all("`")) |>
    dplyr::filter(!rowname |> stringr::str_detect("Intercept")) |>
    dplyr::mutate(`pseudo-R²` = r2[2],
                  Model = nome,
                  `Pr(>|z|)` = dplyr::if_else(`Pr(>|z|)` < 0.01,
                                              "< 0.01",
                                              `Pr(>|z|)` |>
                                                round(2) |>
                                                as.character()),
                  `z value` = `z value` |> round(2)) |>
    dplyr::rename("z" = `z value`,
                  "p" = `Pr(>|z|)`,
                  "Predictor" = rowname) |>
    dplyr::relocate(c(Species, Model), .before = Predictor)

  assign(paste0("resultados_pristimantis_", nome),
         resultados,
         envir = globalenv())

}

purrr::map(c(6, 8, 10:12), rodando_modelos_pristimantis)

ls(pattern = "modelo_pristimantis_") |>
  mget(envir = globalenv())

ls(pattern = "resultados_pristimantis_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

ls(pattern = "resultados_pristimantis_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::filter(!Predictor == "Temperature")

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
    dplyr::mutate(Species = "Adenomera aff. hylaedactyla",
                  rowname = rowname |>
                    stringr::str_remove_all("`")) |>
    dplyr::filter(!rowname |> stringr::str_detect("Intercept")) |>
    dplyr::mutate(`pseudo-R²` = r2[2],
                  Model = nome,
                  `Pr(>|z|)` = dplyr::if_else(`Pr(>|z|)` < 0.01,
                                              "< 0.01",
                                              `Pr(>|z|)` |>
                                                round(2) |>
                                                as.character()),
                  `z value` = `z value` |> round(2)) |>
    dplyr::rename("z" = `z value`,
                  "p" = `Pr(>|z|)`,
                  "Predictor" = rowname) |>
    dplyr::relocate(c(Species, Model), .before = Predictor)

  assign(paste0("resultados_adenomera_", nome),
         resultados,
         envir = globalenv())

}

purrr::map(c(6, 8, 10:12), rodando_modelos_adenomera)

ls(pattern = "modelo_adenomera_") |>
  mget(envir = globalenv())

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
    dplyr::mutate(Species = "Rhinella hoogmoedi",
                  rowname = rowname |>
                    stringr::str_remove_all("`")) |>
    dplyr::filter(!rowname |> stringr::str_detect("Intercept")) |>
    dplyr::mutate(`pseudo-R²` = r2[2],
                  Model = nome,
                  `Pr(>|z|)` = dplyr::if_else(`Pr(>|z|)` < 0.01,
                                              "< 0.01",
                                              `Pr(>|z|)` |>
                                                round(2) |>
                                                as.character()),
                  `z value` = `z value` |> round(2)) |>
    dplyr::rename("z" = `z value`,
                  "p" = `Pr(>|z|)`,
                  "Predictor" = rowname) |>
    dplyr::relocate(c(Species, Model), .before = Predictor)

  assign(paste0("resultados_rhinella_", nome),
         resultados,
         envir = globalenv())

}

purrr::map(c(6, 8, 10:12), rodando_modelos_rhinella)

ls(pattern = "modelo_rhinella_") |>
  mget(envir = globalenv())

ls(pattern = "resultados_rhinella_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

ls(pattern = "resultados_rhinella_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::filter(!Predictor == "Temperature")

# Tabela das estatísticas ----

## Criando o data frame ----

sts_df <- ls(pattern = "^resultados_") |>
  as.data.frame() |>
  dplyr::rename("string" = 1) |>
  dplyr::mutate(especie = string |>
                  stringr::str_replace_all("_", " ") |>
                  stringr::word(2),
                variavel = string |>
                  stringr::str_replace_all("_", " ") |>
                  stringr::word(3)) |>
  dplyr::arrange(especie = especie |>
                   forcats::fct_relevel(c("pristimantis",
                                          "adenomera",
                                          "rhinella")),
                 variavel = variavel |>
                   forcats::fct_relevel(c("Leaf-litter",
                                          "Canopy",
                                          "Edge",
                                          "Elevation",
                                          "Hydric"))) |>
  dplyr::pull(string) |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::rename("β1" = Estimate,
                "SE" = `Std. Error`) |>
  dplyr::mutate("β1 ± EP" = paste0(β1 |> round(4),
                                   " ± ",
                                   SE |> round(4)),
                Predictor = dplyr::case_when(
                  Predictor == "Hydric stream distance" ~ "Water stream distance",
                  .default = Predictor)) |>
  dplyr::select(-c(Model, β1, SE)) |>
  dplyr::relocate(`β1 ± EP`, .after = Predictor)

sts_df

## Tabela flextable ----

sts_df_flex <- sts_df |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(j = c(1, 3), width = 1.5) |>
  flextable::width(j = 6, width = 1) |>
  flextable::italic(j = 1, part = "body") |>
  flextable::fontsize(size = 12, part = "all") |>
  flextable::bg(part = "all", bg = "white")

sts_df_flex

sts_df_flex_destacado <- sts_df |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(j = c(1, 3), width = 1.5) |>
  flextable::width(j = 6, width = 1) |>
  flextable::italic(j = 1, part = "body") |>
  flextable::fontsize(size = 12, part = "all") |>
  flextable::bg(part = "all", bg = "white") |>
  flextable::bg(i = ~abs(z) > 1.96, bg = "gray")

sts_df_flex_destacado

## Exportando a tabela ----

sts_df_flex |>
  flextable::save_as_docx(path = "tabela_estatisticas_modelos_ocupacao.docx")

# Gráficos ----

## Preditores significativos para cada espécie ----

prediotores_pristimantis <- sts_df |>
  dplyr::filter(Species == "Pristimantis ramagii" & abs(z) > 1.96) |>
  dplyr::pull(Predictor)

prediotores_pristimantis

prediotores_adenomera <- sts_df |>
  dplyr::filter(Species == "Adenomera aff. hylaedactyla" & abs(z) > 1.96) |>
  dplyr::pull(Predictor)

prediotores_adenomera

prediotores_rhinella <- sts_df |>
  dplyr::filter(Species == "Rhinella hoogmoedi" & abs(z) > 1.96) |>
  dplyr::pull(Predictor)

prediotores_rhinella

## Linhas de tendência ----

### Criando as linhas de tendência ----

criar_linhas <- function(id){

  especie <- modelo[id] |>
    names() |>
    stringr::str_replace_all("_", " ") |>
    stringr::word(2)

  tendencia <- ggeffects::ggpredict(model = modelo[id],
                                    terms = variavel[id]) |>
    as.data.frame() |>
    dplyr::select(1:2) |>
    dplyr::mutate(Preditor = variavel[id],
                  Species = especie) |>
    dplyr::rename("Valor preditor" = 1,
                  "Predicted" = 2)

  nome <- variavel[id] |>
    stringr::word(1)

  assign(paste0("tendencia_", nome, "_", especie),
         tendencia,
         envir = globalenv())

}

modelo <- ls(pattern = "modelo_") |>
  mget(envir = globalenv())

modelo

variavel <- df_ocupacao |>
  dplyr::select(c(6, 8, 10:12)) |>
  names() |>
  sort() |>
  rep(3)

variavel

purrr::map(1:15, criar_linhas)

### Unindo os dados ----

df_tendencia <- ls(pattern = "tendencia_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(Preditor = dplyr::case_when(
                  Preditor == "Hydric stream distance" ~ "Water stream distance",
                  .default = Predictor),
                Preditor = Preditor |>
                  forcats::fct_relevel(c("Leaf-litter depth",
                                         "Canopy openness",
                                         "Edge distance",
                                         "Elevation",
                                         "Water stream distance"))) |>
  dplyr::group_by(Preditor, Species) |>
  dplyr::slice(c(1, n()))

df_tendencia

## Pristimantis ramagii ----

df_ocupacao |>
  tidyr::pivot_longer(cols = c(6, 8, 10:12),
                      names_to = "Preditor",
                      values_to = "Valor preditor") |>
  dplyr::mutate(Preditor = Preditor |>
                  forcats::fct_relevel(c("Leaf-litter depth",
                                         "Canopy openness",
                                         "Edge distance",
                                         "Elevation",
                                         "Hydric stream distance"))) |>
  ggplot(aes(`Valor preditor`, `Pristimantis ramagii`)) +
  geom_point(color = "black",
             size = 3.5) +
  facet_wrap(~Preditor, scales = "free_x") +
  geom_line(data = df_tendencia |>
              dplyr::filter(Species == "pristimantis" &
                              Preditor %in% prediotores_pristimantis),
            aes(`Valor preditor`, Predicted), color = "blue", linewidth = 1) +
  labs(x = "Predictor value",
       y = "<i>Pristimantis ramagii</i> abundance") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        axis.title.y = ggtext::element_markdown(color = "black", size = 15),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 20),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "none",
        title = element_text(color = "black", size = 15),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10,
                 width = 12)

ggsave(filename = "modelo_abundancia_pristimantis_multiplo_ggeffects.png",
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
                                         "Hydric stream distance"))) |>
  ggplot(aes(`Valor preditor`, `Adenomera hylaedactyla`)) +
  geom_point(color = "black",
             size = 3.5) +
  facet_wrap(~Preditor, scales = "free_x") +
  geom_line(data = df_tendencia |>
              dplyr::filter(Species == "Adenomera" &
                              Preditor %in% prediotores_adenomera),
            aes(`Valor preditor`, Preditor)) +
  labs(x = "Predictor value",
       y = "<i>Adenomera</i> aff. <i>hylaedactyla</i> abundance") +
  scale_y_continuous(limits = c(5, 17.5)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        axis.title.y = ggtext::element_markdown(color = "black", size = 15),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 20),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "none",
        title = element_text(color = "black", size = 15),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10,
                 width = 12)

ggsave(filename = "modelo_abundancia_adenomera_multiplo_ggeffect.png",
       height = 10, width = 12)

## Rhinella hoogmoedi ----

df_ocupacao |>
  tidyr::pivot_longer(cols = c(6, 8, 10:12),
                      names_to = "Preditor",
                      values_to = "Valor preditor") |>
  dplyr::mutate(Preditor = paste0(Preditor, ""),
                Preditor = Preditor |>
                  forcats::fct_relevel(c("Leaf-litter depth",
                                         "Canopy openness",
                                         "Edge distance",
                                         "Elevation",
                                         "Hydric stream distance"))) |>
  ggplot(aes(`Valor preditor`, `Rhinella hoogmoedi`)) +
  geom_point(color = "black",
             size = 3.5) +
  facet_wrap(~Preditor, scales = "free_x") +
  geom_line(data = df_tendencia |>
              dplyr::filter(Species == "rhinella" &
                              Preditor %in% prediotores_rhinella),
            aes(`Valor preditor`, Predicted), color = "blue", linewidth = 1) +
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

ggsave(filename = "modelo_abundancia_rhinella_multiplo_ggeffect.png",
       height = 10, width = 12)
