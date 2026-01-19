# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(vegan)

library(DHARMa)

library(performance)

library(ggview)

library(flextable)

library(rsq)

library(ggtext)

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
                   by = "Unidade Amostral")

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

matriz_pocas <- ambientais |>
  dplyr::select(`Área das poças`) |>
  vegan::vegdist(method = "euclidean") |>
  as.numeric()

matriz_pocas

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

df_beta <- tibble::tibble(Composição = matriz_comp,
                          `Abertura de dossel` = matriz_dossel,
                          `Área de Poças` = matriz_pocas,
                          `Altura da Serrapilheira` = matriz_serrapilheira,
                          `Distância dos corpos hidricos` = matriz_distancia,
                          Temperatura = matriz_temperatura,
                          `Distância da borda` = matriz_borda,
                          `Altitude` = matriz_altitude)

df_beta

df_beta |> dplyr::glimpse()

# Modelos lineares ----

## Multicolinearidade ----

df_alfa |>
  dplyr::select(3, 4, 6, 8, 10) |>
  cor(method = "spearman")

## Diversidade alfa ----

### Múltiplos modelos ----

modelos_diversidade <- function(id){

  id <- as.integer(id)

  nome <- df_alfa[id] |> names()

  paste0("Criando o modelo para: ",
         nome) |>
    crayon::green() |>
    message()

  df_trat <<- df_alfa |> dplyr::select(2, id)

  modelo <- lm(`Q = 1` ~ .,
               data = df_trat)

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

purrr::walk(c(3, 4, 6, 8, 10), modelos_diversidade)

ls(pattern = "modelo_alfa_") |>
  mget(envir = globalenv())

ls(pattern = "resultados_alfa_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

### Modelo múltiplo ----

#### Criando o modelo ----

modelo_q1 <- lm(`Q = 1` ~ .,
                data = df_alfa |>
                   dplyr::select(2:4, 6, 8, 10))

#### Pressupostos do modelo ----

modelo_q1 |> performance::check_heteroscedasticity()

modelo_q1 |> performance::check_normality()

modelo_q1 |> performance::check_model(check = c("vif",
                                                "qq",
                                                "normality",
                                                "homogeneity"))

#### Avaliando o modelo ----

modelo_q1 |> summary()

qt(p = 0.05, df = 6, lower.tail = FALSE)

qf(p = 0.05, df1 = 4, df2 = 6, lower.tail = FALSE)

#### Pseudo-R² ----

modelo_q1 |> rsq::rsq()

modelo_q1 |> rsq::rsq(adj = TRUE)

#### Tabela ----

##### Dataframe da tabelas ----

ls(pattern = "resultados_alfa_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

df_flex1 <- ls(pattern = "resultados_alfa_") |>
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
                "EP" = `Std. Error`,
                "t" = `t value`,
                "p" = `Pr(>|t|)`) |>
  tidyr::unite(β1:EP,
               sep = " ± ",
               col = "β1 ± EP") |>
  dplyr::relocate(DF, .before = p)

df_flex1

##### Criando a tabela ----

flex_q1 <- df_flex1 |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 1.5, j = 2) |>
  flextable::add_footer_lines("t-crítico = 1.83") |>
  flextable::fontsize(size = 12, part = "all") |>
  flextable::bg(bg = "white", part = "all")

flex_q1

##### Exportando a tabela ----

flex_q1 |>
  flextable::save_as_docx(path = "tabela_q1.docx")

## Dataframe de estatísticas usadas no gráfico ----

### Valor medano das variáveis ----

medias_alfa <- df_alfa |>
  dplyr::select(3:4, 6, 8, 10) |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "Preditor",
                      values_to = "Valor") |>
  dplyr::arrange(Preditor |> forcats::fct_relevel(df_flex1$Preditor)) |>
  dplyr::summarise(`Valor Preditor` = mean(c(min(Valor), max(Valor))),
                   .by = Preditor)

medias_alfa

### Dataframe ----

df_q1_estatisticas <- df_flex1 |>
  dplyr::mutate(`Q = 1` = 4.3,
                estatistica = paste0("β1 ± EP = ",
                                     `β1 ± EP`,
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
                                     `R² ajustado`)) |>
  dplyr::select(-c(2:10)) |>
  dplyr::left_join(medias_alfa,
                   by = "Preditor")

df_q1_estatisticas

### Gráfico -----

df_alfa |>
  tidyr::pivot_longer(cols = c(3, 4, 6, 8, 10),
                      names_to = "Preditor",
                      values_to = "Valor Preditor") |>
  dplyr::mutate(Preditor = Preditor |>
                  stringr::str_replace("hidrico", "hídrico")) |>
  ggplot(aes(`Valor Preditor`,`Q = 1`,  fill = Preditor)) +
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
  #scale_fill_manual(values = c("green4", "gold", "orange3", "skyblue", "royalblue")) +
  labs(title = "t-crítico = 1.83, F-crítico = 5.12") +
  scale_y_continuous(limits = c(2.5, 4.45)) +
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

ggsave(filename = "grafico_pontos_q1.png", height = 10, width = 12)

## Diversidade beta -----

### Multicolinearidade ----

df_beta |>
  dplyr::select(2:5, 8) |>
  cor(method = "spearman")

### Criando o modelo ----

modelo_beta <- glmmTMB::glmmTMB(Composição ~ `Abertura de dossel` +
                                  `Área de Poças` +
                                  `Altura da Serrapilheira` +
                                  `Distância dos corpos hidricos` +
                                  Altitude,
                                data = df_beta,
                                family = glmmTMB::beta_family())

### Pressupostos do modelo ----

modelo_beta |>
  DHARMa::simulateResiduals(plot = TRUE)

modelo_beta |>
  performance::check_model(check = c("vif",
                                     "qq",
                                     "normality",
                                     "homogeneity"))

### Avaliando o modelo ----

modelo_beta |>
  summary()

### Pseudo-R² ----

modelo_beta |> performance::r2()

### Tabela ----

#### Dataframe da tabelas ----

df_flexbeta <- modelo_beta |>
  summary() %>%
  .$coefficients  %>%
  .$cond

nomes_var <- df_flexbeta |> rownames()

nomes_var

df_flexbeta_trat <- df_flexbeta |>
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
  dplyr::relocate(DF, .before = p)

df_flexbeta_trat

#### Criando a tabela ----

flex_beta <- df_flexbeta_trat |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 1.5, j = 2) |>
  flextable::add_footer_lines("z-crítico = 1.96, pseudo-R² = 0.20") |>
  flextable::fontsize(size = 12, part = "all")

flex_beta

#### Exportando a tabela ----

flex_beta |>
  flextable::save_as_docx(path = "tabela_beta.docx")

## Dataframe de estatísticas usadas no gráfico ----

### Valor medano das variáveis ----

medias_beta <- df_beta |>
  dplyr::select(2:5, 8) |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "Preditor",
                      values_to = "Valor") |>
  dplyr::mutate(Preditor = Preditor |> stringr::str_replace_all("hidricos",
                                                                "hídricos")) |>
  dplyr::arrange(Preditor |> forcats::fct_relevel(df_flexbeta_trat$Preditor)) |>
  dplyr::summarise(`Valor Preditor` = mean(c(min(Valor), max(Valor))),
                   .by = Preditor)

medias_beta

### Dataframe ----

df_beta_estatisticas <- df_flexbeta_trat |>
  dplyr::mutate(Composição = 0.52,
                DF = 51,
                estatistica = paste0("β1 ± EP = ",
                                     `β1 ± EP`,
                                     "<br>z = ",
                                     z,
                                     "<sub>",
                                     DF,
                                     "</sub>, p = ",
                                     p |> round(3))) |>
  dplyr::select(-c(2:5)) |>
  dplyr::left_join(medias_beta,
                   by = "Preditor")

df_beta_estatisticas

### Gráfico ----

df_beta |>
  tidyr::pivot_longer(cols = c(`Abertura de dossel`,
                               `Área de Poças`,
                               `Altura da Serrapilheira`,
                               `Distância dos corpos hidricos`,
                               Altitude),
                      names_to = "Preditor",
                      values_to = "Valor Preditor") |>
  dplyr::mutate(Preditor = Preditor |>
                  stringr::str_replace("hidrico", "hídrico")) |>
  ggplot(aes(`Valor Preditor`, Composição, fill = Preditor, color = Preditor)) +
  geom_point(color = "black",
             size = 3.5,
             stroke = 1) +
  geom_smooth(data = . %>%
                dplyr::filter(Preditor %in% c("Abertura de dossel",
                                              "Altitude")),
              method = "lm",
              se = FALSE) +
  facet_wrap(~Preditor, scales = "free_x") +
  ggtext::geom_richtext(data = df_beta_estatisticas,
                        aes(label = estatistica),
                        color = "black",
                        fontface = "bold",
                        label.colour = "transparent",
                        fill = "transparent",
                        size = 5) +
  #scale_fill_manual(values = c("green4", "gold", "orange3", "skyblue", "royalblue")) +
  scale_color_manual(values = c("black", "black")) +
  scale_y_continuous(limits = c(0.1, 0.53)) +
  labs(x = "Distância preditora",
       y = "Distância de composição",
       title = "z-crítico = 1.96, pseudo-R² ajustado = 0.20") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 15),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "none",
        title = element_text(color = "black", size = 15),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "grafico_pontos_beta.png", height = 10, width = 12)
