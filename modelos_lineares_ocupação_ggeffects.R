# Pacotes -----

library(readxl)

library(tidyverse)

library(magrittr)

library(crayon)

library(DHARMa)

library(performance)

library(broom)

library(flextable)

library(ggeffects)

library(ggtext)

library(ggview)

# Dados ----

## Abundância de espécies -----

### Importando ----

especies <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/levantamento_anuros.xlsx")

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

ambientais <-readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/matriz_ambientais.xlsx")

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
  dplyr::rename("Canopy openness" = 5,
                "Leaf-litter depth" = 7,
                "Temperature" = 8,
                "Hydric stream distance" = 9,
                "Edge distance" = 10,
                "Elevation" = 11)

df_ocupacao

df_ocupacao |> glimpse()

df_ocupacao |>
  dplyr::select(c(5, 7, 9:11)) |>
  glimpse()

# Modelos lineares ----

## Pristimantis ramagii ----

modelos_pristimantis <- purrr::map(c(5, 7, 9:11), \(id){

  modelo <- glm(`Pristimantis ramagii` ~ .,
                data = df_ocupacao[, c(2, id, 8)],
                family = poisson(link = "log"))

  })

modelos_pristimantis

purrr::map2(c(5, 7, 9:11), modelos_pristimantis, \(id, modelo){

  nome <- df_ocupacao[, id] |> names()

  paste0("Criando o modelo de Pristimantis para: ",
         nome) |>
    crayon::green() |>
    message()

  nome <- df_ocupacao[, id] |>
    names() |>
    stringr::word(1)

  paste0("pressupostos do modelo de Pristimantis para: ",
         nome) |>
    crayon::green() |>
    message()

  modelo |>
    DHARMa::simulateResiduals(plot = TRUE) |>
    print()

  r2 <- modelo |>
    performance::r2_mcfadden() |>
    as.numeric() |>
    round(2)

  paste0("pseudo-R²: ",
         r2) |>
    crayon::green() |>
    message()

  })

resultados_pristimantis <- map2_dfr(
  c(5, 7, 9:11),
  modelos_pristimantis,
  \(id, modelo){

  nome <- df_ocupacao[, id] |>
    names()

  r2 <- modelo |>
    performance::r2_mcfadden() |>
    as.numeric() |>
    round(2)

  modelo |>
    broom::tidy() |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::rename("Predictor" = term,
                  "z" = statistic,
                  "p" = p.value) |>
    dplyr::mutate(Species = "Pristimantis ramagii") |>
    dplyr::mutate(`pseudo-R²` = r2[2],
                  Model = nome,
                  p = dplyr::if_else(p < 0.01,
                                     "< 0.01",
                                     p |> round(2) |> as.character())) |>
    dplyr::relocate(c(Species, Model), .before = Predictor)

  },
  .progress = TRUE)

resultados_pristimantis

## Adenomera Hylaedactyla ----

modelos_adenomera <- purrr::map(c(5, 7, 9:11), \(id){

  modelo <- glm(`Adenomera hylaedactyla` ~ .,
                data = df_ocupacao[, c(3, id, 8)],
                family = poisson(link = "log"))

})

modelos_adenomera

purrr::map2(c(5, 7, 9:11), modelos_adenomera, \(id, modelo){

  nome <- df_ocupacao[, id] |> names()

  paste0("Criando o modelo de Adenomera para: ",
         nome) |>
    crayon::green() |>
    message()

  nome <- df_ocupacao[, id] |>
    names() |>
    stringr::word(1)

  paste0("pressupostos do modelo de Adenomera para: ",
         nome) |>
    crayon::green() |>
    message()

  modelo |>
    DHARMa::simulateResiduals(plot = TRUE) |>
    print()

  r2 <- modelo |>
    performance::r2_mcfadden() |>
    as.numeric() |>
    round(2)

  paste0("pseudo-R²: ",
         r2) |>
    crayon::green() |>
    message()

})

resultados_adenomera <- map2(c(5, 7, 9:11), modelos_adenomera, \(id, modelo){

  nome <- df_ocupacao[, id] |>
    names()

  r2 <- modelo |>
    performance::r2_mcfadden() |>
    as.numeric() |>
    round(2)

  modelo |>
    broom::tidy() |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::rename("Predictor" = term,
                  "z" = statistic,
                  "p" = p.value) |>
    dplyr::mutate(Species = "Adenomera aff. hylaedactyla",
                  `pseudo-R²` = r2[2],
                  Model = nome,
                  p = dplyr::if_else(p < 0.01,
                                     "< 0.01",
                                     p |> round(2) |> as.character())) |>
    dplyr::relocate(c(Species, Model), .before = Predictor)

}) |>
  dplyr::bind_rows()

resultados_adenomera

## Rhinella hoogmoedi ----

modelos_rhinella <- purrr::map(c(5, 7, 9:11), \(id){

  modelo <- glm(`Rhinella hoogmoedi` ~ .,
                data = df_ocupacao[, c(4, id, 8)],
                family = poisson(link = "log"))

})

modelos_rhinella

purrr::map2(c(5, 7, 9:11), modelos_rhinella, \(id, modelo){

  nome <- df_ocupacao[, id] |> names()

  paste0("Criando o modelo de Rhinella para: ",
         nome) |>
    crayon::green() |>
    message()

  nome <- df_ocupacao[, id] |>
    names() |>
    stringr::word(1)

  paste0("pressupostos do modelo de Rhinella para: ",
         nome) |>
    crayon::green() |>
    message()

  modelo |>
    DHARMa::simulateResiduals(plot = TRUE) |>
    print()

  r2 <- modelo |>
    performance::r2_mcfadden() |>
    as.numeric() |>
    round(2)

  paste0("pseudo-R²: ",
         r2) |>
    crayon::green() |>
    message()

})

resultados_rhinella <- map2(c(5, 7, 9:11), modelos_rhinella, \(id, modelo){

  nome <- df_ocupacao[, id] |>
    names()

  r2 <- modelo |>
    performance::r2_mcfadden() |>
    as.numeric() |>
    round(2)

  modelo |>
    broom::tidy() |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::rename("Predictor" = term,
                  "z" = statistic,
                  "p" = p.value) |>
    dplyr::mutate(Species = "Rhinella hoogmoedi",
                  `pseudo-R²` = r2[2],
                  Model = nome,
                  p = dplyr::if_else(p < 0.01,
                                     "< 0.01",
                                     p |> round(2) |> as.character())) |>
    dplyr::relocate(c(Species, Model), .before = Predictor)

  }) |>
  dplyr::bind_rows()

resultados_rhinella

# Tabela das estatísticas ----

## Criando o data frame ----

sts_df <- ls(pattern = "^resultados_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::arrange(Species = Species |>
                   forcats::fct_relevel(c("Pristimantis ramagii",
                                          "Adenomera aff. hylaedactyla",
                                          "Rhinella hoogmoedi")),
                 Model = Model |>
                   forcats::fct_relevel(c("Leaf-litter depth",
                                          "Canopy openness",
                                          "Edge distance",
                                          "Elevation",
                                          "Hydric stream distance"))) |>
  dplyr::rename("β1" = estimate,
                "SE" = std.error) |>
  dplyr::mutate("β1 ± SE" = paste0(β1 |> round(4),
                                   " ± ",
                                   SE |> round(4)),
                Predictor = Predictor |>
                  stringr::str_replace("Hydric", "Water") |>
                  stringr::str_remove_all("`")) |>
  dplyr::select(-c(Model, β1, SE)) |>
  dplyr::relocate(`β1 ± SE`, .after = Predictor) |>
  dplyr::mutate(Species = dplyr::case_when(Predictor == "Temperature" ~ NA,
                                           .default = Species),
                `pseudo-R²` = dplyr::case_when(Predictor == "Temperature" ~ NA,
                                               .default = `pseudo-R²`))

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
  dplyr::filter(Species == "Pristimantis ramagii" &
                  abs(z) > 1.96 &
                  Predictor != "Temperature") |>
  dplyr::pull(Predictor)

prediotores_pristimantis

prediotores_adenomera <- sts_df |>
  dplyr::filter(Species == "Adenomera aff. hylaedactyla" &
                  abs(z) > 1.96 &
                  Predictor != "Temperature") |>
  dplyr::pull(Predictor)

prediotores_adenomera

prediotores_rhinella <- sts_df |>
  dplyr::filter(Species == "Rhinella hoogmoedi" &
                  abs(z) > 1.96 &
                  Predictor != "Temperature") |>
  dplyr::pull(Predictor)

prediotores_rhinella

## Linhas de tendência ----

### Criando as linhas de tendência ----

modelos <- c(modelos_pristimantis,
             modelos_adenomera,
             modelos_rhinella)

modelos

sps <- c("Pristimantis ramagii",
                    "Adenomera hylaedactyla",
                    "Rhinella hoogmoedi") |>
  rep(each = 5)

sps

variavel <- df_ocupacao |>
  dplyr::select(c(5, 7, 9:11)) |>
  names() |>
  rep(3)

variavel

df_tendencia <- purrr::pmap(list(modelos, variavel, sps), \(modelos, variavel, sps){

  ggeffects::ggpredict(model = modelos,
                       terms = variavel) |>
    as.data.frame() |>
    dplyr::select(1:2) |>
    dplyr::mutate(Preditor = variavel,
                  Species = sps) |>
    dplyr::rename("Valor preditor" = 1,
                  "Predicted" = 2)

  }) |>
  dplyr::bind_rows() |>
  dplyr::mutate(Preditor = dplyr::case_when(
                  Preditor == "Hydric stream distance" ~ "Water stream distance",
                  .default = Preditor),
                Preditor = Preditor |>
                  forcats::fct_relevel(c("Leaf-litter depth",
                                         "Canopy openness",
                                         "Edge distance",
                                         "Elevation",
                                         "Water stream distance"))) |>
  dplyr::group_by(Preditor, Species) |>
  dplyr::slice(c(1, n())) |>
  dplyr::ungroup()

df_tendencia

## Pristimantis ramagii ----

df_ocupacao |>
  tidyr::pivot_longer(cols = c(5, 7, 9:11),
                      names_to = "Preditor",
                      values_to = "Valor preditor") |>
  dplyr::mutate(Preditor = dplyr::case_when(
    Preditor == "Hydric stream distance" ~ "Water stream distance",
    .default = Preditor),
                Preditor = Preditor |>
                  forcats::fct_relevel(c("Leaf-litter depth",
                                         "Canopy openness",
                                         "Edge distance",
                                         "Elevation",
                                         "Water stream distance"))) |>
  ggplot(aes(`Valor preditor`, `Pristimantis ramagii`)) +
  geom_point(color = "black",
             size = 3.5) +
  facet_wrap(~Preditor, scales = "free_x") +
  geom_line(data = df_tendencia |>
              dplyr::filter(Species == "Pristimantis ramagii" &
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

ggsave(filename = "C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/modelo_abundancia_pristimantis_multiplo_ggeffects.png",
       height = 10, width = 12)

## Adenomera hylaedactyla ----

df_ocupacao |>
  tidyr::pivot_longer(cols = c(5, 7, 9:11),
                      names_to = "Preditor",
                      values_to = "Valor preditor") |>
  dplyr::mutate(Preditor = dplyr::case_when(
    Preditor == "Hydric stream distance" ~ "Water stream distance",
    .default = Preditor),
                Preditor = Preditor |>
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
              dplyr::filter(Species == "Adenomera hylaedactyla" &
                              Preditor %in% prediotores_adenomera),
            aes(`Valor preditor`, Predicted)) +
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

ggsave(filename = "C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/modelo_abundancia_adenomera_multiplo_ggeffect.png",
       height = 10, width = 12)

## Rhinella hoogmoedi ----

df_ocupacao |>
  tidyr::pivot_longer(cols = c(5, 7, 9:11),
                      names_to = "Preditor",
                      values_to = "Valor preditor") |>
  dplyr::mutate(Preditor = paste0(Preditor, ""),
                Preditor = dplyr::case_when(
                  Preditor == "Hydric stream distance" ~ "Water stream distance",
                  .default = Preditor),
                Preditor = Preditor |>
                  forcats::fct_relevel(c("Leaf-litter depth",
                                         "Canopy openness",
                                         "Edge distance",
                                         "Elevation",
                                         "Water stream distance"))) |>
  ggplot(aes(`Valor preditor`, `Rhinella hoogmoedi`)) +
  geom_point(color = "black",
             size = 3.5) +
  facet_wrap(~Preditor, scales = "free_x") +
  geom_line(data = df_tendencia |>
              dplyr::filter(Species == "Rhinella hoogmoedi" &
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

ggsave(filename = "C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/modelo_abundancia_rhinella_multiplo_ggeffect.png",
       height = 10, width = 12)

# Gráficos com as estatísticas ----

## Estatísticas ----

sts_df_2 <- sts_df |>
  tidyr::drop_na() |>
  dplyr::mutate(Abundancia = dplyr::case_when(
    Species == "Pristimantis ramagii" ~ 30,
    Species == "Adenomera aff. hylaedactyla" ~ 17,
    Species == "Rhinella hoogmoedi" ~ 11.5
  ),
  p = dplyr::case_when(
    !p |> stringr::str_detect("<") ~ paste0("= ", p),
    .default = p
  ),
  sts = paste0("β1 ± SE = ",
               `β1 ± SE`,
               "<br>z = ",
               z |> round(2),
               ", p ",
               p,
               ", pseudo-R² = ",
               `pseudo-R²`)) |>
  rename("Preditor" = Predictor) |>
  dplyr::left_join(df_ocupacao |>
                     tidyr::pivot_longer(cols = c(5, 7, 9:11),
                                         names_to = "Preditor",
                                         values_to = "Valor preditor") |>
                     dplyr::summarise(`Valor preditor` = `Valor preditor` |>
                                        range() |>
                                        mean(),
                                      .by = Preditor) |>
                     dplyr::mutate(
                       Preditor = dplyr::case_when(
                         Preditor == "Hydric stream distance" ~ "Water stream distance",
                         .default = Preditor
                       )),
                   by = "Preditor") |>
  dplyr::mutate(Preditor = paste0(Preditor, ""),
                Preditor = dplyr::case_when(
                  Preditor == "Leaf-litter depth" ~ "Altura da serrapilheira",
                  Preditor == "Canopy openness" ~ "Abertura do dossel",
                  Preditor == "Edge distance" ~ "Distância da borda",
                  Preditor == "Elevation" ~ "Elevação",
                  Preditor == "Water stream distance" ~ "Distância dos corpos hídricos"),
                Preditor = Preditor |>
                  forcats::fct_relevel(c("Altura da serrapilheira",
                                         "Abertura do dossel",
                                         "Distância da borda",
                                         "Elevação",
                                         "Distância dos corpos hídricos"))) |>
  as.data.frame()

sts_df_2

## Data frame traduzido ----

### Dados de abundância ----

df_ocupacao_traduzido <- df_ocupacao |>
  tidyr::pivot_longer(cols = c(5, 7, 9:11),
                      names_to = "Preditor",
                      values_to = "Valor preditor") |>
  dplyr::mutate(Preditor = paste0(Preditor, ""),
                Preditor = dplyr::case_when(
                  Preditor == "Leaf-litter depth" ~ "Altura da serrapilheira",
                  Preditor == "Canopy openness" ~ "Abertura do dossel",
                  Preditor == "Edge distance" ~ "Distância da borda",
                  Preditor == "Elevation" ~ "Elevação",
                  Preditor == "Hydric stream distance" ~ "Distância dos corpos hídricos"),
                Preditor = Preditor |>
                  forcats::fct_relevel(c("Altura da serrapilheira",
                                         "Abertura do dossel",
                                         "Distância da borda",
                                         "Elevação",
                                         "Distância dos corpos hídricos"))) |>
  as.data.frame()

df_ocupacao_traduzido

### Tendencias ----

df_tendencia_traduzido <- df_tendencia |>
  dplyr::mutate(Preditor = paste0(Preditor, ""),
                Preditor = dplyr::case_when(
                  Preditor == "Leaf-litter depth" ~ "Altura da serrapilheira",
                  Preditor == "Canopy openness" ~ "Abertura do dossel",
                  Preditor == "Edge distance" ~ "Distância da borda",
                  Preditor == "Elevation" ~ "Elevação",
                  Preditor == "Water stream distance" ~ "Distância dos corpos hídricos"),
                Preditor = Preditor |>
                  forcats::fct_relevel(c("Altura da serrapilheira",
                                         "Abertura do dossel",
                                         "Distância da borda",
                                         "Elevação",
                                         "Distância dos corpos hídricos"))) |>
  as.data.frame()

df_tendencia_traduzido

## Pristimantis ramagii ----

df_ocupacao_traduzido |>
  ggplot(aes(`Valor preditor`, `Pristimantis ramagii`)) +
  geom_point(color = "black",
             size = 3.5) +
  ggtext::geom_richtext(data = sts_df_2 |>
                          dplyr::filter(Species == "Pristimantis ramagii"),
                        aes(`Valor preditor`, Abundancia, label = sts),
                        color = "black",
                        fill = "transparent",
                        label.colour = "transparent",
                        fontface = "bold",
                        size = 5) +
  facet_wrap(~Preditor, scales = "free_x") +
  geom_line(data = df_tendencia_traduzido |>
              dplyr::filter(Species == "Pristimantis ramagii" &
                              Preditor %in% c("Altura da serrapilheira",
                                              "Abertura do dossel",
                                              "Distância dos corpos hídricos")),
            aes(`Valor preditor`, Predicted), color = "blue", linewidth = 1) +
  labs(x = "Valor preditor",
       y = "Abundância de <i>Pristimantis ramagii</i>") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        axis.title.y = ggtext::element_markdown(color = "black", size = 20),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 20),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "none",
        title = element_text(color = "black", size = 20),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10,
                 width = 12)

ggsave(filename = "C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/apresentação/modelo_abundancia_pristimantis_ramagii.png",
       height = 10,
       width = 12)

## Adenomera aff. hylaedactyla ----

df_ocupacao_traduzido |>
  ggplot(aes(`Valor preditor`, `Adenomera hylaedactyla`)) +
  geom_point(color = "black",
             size = 3.5) +
  ggtext::geom_richtext(data = sts_df_2 |>
                          dplyr::filter(Species == "Adenomera aff. hylaedactyla"),
                        aes(`Valor preditor`, Abundancia, label = sts),
                        color = "black",
                        fill = "transparent",
                        label.colour = "transparent",
                        fontface = "bold",
                        size = 5) +
  facet_wrap(~Preditor, scales = "free_x") +
  labs(x = "Valor preditor",
       y = "Abundância de <i>Adenomera</i> aff. <i>hylaedactyla</i>") +
  scale_y_continuous(limits = c(5, 17.65)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        axis.title.y = ggtext::element_markdown(color = "black", size = 20),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 20),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "none",
        title = element_text(color = "black", size = 20),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10,
                 width = 12)

ggsave(filename = "C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/apresentação/modelo_abundancia_adenomera_hylaedactyla.png",
       height = 10,
       width = 12)

### Rhinella hoogmoedi ----

df_ocupacao_traduzido |>
  ggplot(aes(`Valor preditor`, `Rhinella hoogmoedi`)) +
  geom_point(color = "black",
             size = 3.5) +
  ggtext::geom_richtext(data = sts_df_2 |>
                          dplyr::filter(Species == "Rhinella hoogmoedi"),
                        aes(`Valor preditor`, Abundancia, label = sts),
                        color = "black",
                        fill = "transparent",
                        label.colour = "transparent",
                        fontface = "bold",
                        size = 5) +
  facet_wrap(~Preditor, scales = "free_x") +
  geom_line(data = df_tendencia_traduzido |>
              dplyr::filter(Species == "Rhinella hoogmoedi" &
                              Preditor %in% c("Abertura do dossel",
                                              "Distância da borda",
                                              "Elevação")),
            aes(`Valor preditor`, Predicted), color = "blue", linewidth = 1) +
  labs(x = "Valor preditor",
       y = "Abundância de <i>Rhinella hoogmoedi</i>") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        axis.title.y = ggtext::element_markdown(color = "black", size = 20),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 20),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "none",
        title = element_text(color = "black", size = 15),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10,
                 width = 12)

ggsave(filename = "C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/apresentação/modelo_abundancia_rhinella_hoogmoedi.png",
       height = 10,
       width = 12)
