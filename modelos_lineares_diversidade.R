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

### Tratando ----

nomes_linhas <- especies$`Unidade Amostral`

especies <- especies |>
  dplyr::select_if(is.numeric)

row.names(especies) <- nomes_linhas

especies

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
  vegan::renyi(scales = 1, hill = TRUE) |>
  dplyr::as_data_frame() |>
  dplyr::mutate(`Unidade Amostral` = nomes_linhas) |>
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
  dplyr::select(3, 4, 6, 8) |>
  cor(method = "spearman")

## Diversidade alfa ----

### Criando o modelo ----

modelo_q1 <- glm(`Q = 1` ~ .,
                 data = df_alfa |>
                   dplyr::select(2:4, 6, 8),
                 family = Gamma(link = "identity"))

### Pressupostos do modelo ----

modelo_q1 |> DHARMa::simulateResiduals(plot = TRUE)

modelo_q1 |> performance::check_model(check = c("vif",
                                                "qq",
                                                "normality",
                                                "homogeneity"))

### Avaliando o modelo ----

modelo_q1 |> summary()

qt(p = 0.05, df = 6, lower.tail = FALSE)

### Pseudo-R² ----

modelo_q1 |> rsq::rsq()

modelo_q1 |> rsq::rsq(adj = TRUE)

### Tabela ----

#### Dataframe da tabelas ----

df_flex1 <- modelo_q1 |>
  summary() %>%
  .$coefficients

df_flex1

nomes_var <- df_flex1 |> rownames()

nomes_var

df_flex1_trat <- df_flex1 |>
  tibble::as_tibble() |>
  dplyr::mutate(Preditor = nomes_var |>
                  stringr::str_remove_all("`") |>
                  stringr::str_replace("hidrico", "hídrico"),
                Estimate = Estimate |> round(4),
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
               col = "β1 ± EP")

df_flex1_trat

#### Criando a tabela ----

flex_q1 <- df_flex1_trat |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 1.5) |>
  flextable::add_footer_lines("t-crítico = 1.94, AIC = 6.5, pseudo-R² = 0.69, pseudo-R² ajustado = 0.37") |>
  flextable::fontsize(size = 12, part = "all")

flex_q1

#### Exportando a tabela ----

flex_q1 |>
  flextable::save_as_docx(path = "tabela_q1.docx")

### Multicolinearidade ----

df_beta |>
  dplyr::select(2:5) |>
  cor(method = "spearman")

### Gráfico -----

df_q1_estatisticas <- df_flex1_trat |>
  dplyr::mutate(`Valor Preditor` = c(6, 0.155, 5, 350),
                `Q = 1` = 3.9,
                df = 6,
                estatistica = paste0("β1 ± EP = ",
                                     `β1 ± EP`,
                                     "<br>t = ",
                                     t,
                                     "<sub>",
                                     df,
                                     "</sub>, p = ",
                                     p)) |>
  dplyr::select(-c(2:4))

df_q1_estatisticas

df_alfa |>
  tidyr::pivot_longer(cols = c(3, 4, 6, 8),
                      names_to = "Preditor",
                      values_to = "Valor Preditor") |>
  dplyr::mutate(Preditor = Preditor |>
                  stringr::str_replace("hidrico", "hídrico")) |>
  ggplot(aes(`Valor Preditor`,`Q = 1`,  fill = Preditor)) +
  geom_point(shape = 21,
             color = "black",
             size = 3.5,
             stroke = 1) +
  geom_smooth(data = . %>%
                dplyr::filter(Preditor == "Distância dos corpos hídricos"),
              method = "lm",
              se = FALSE) +
  facet_wrap(~Preditor, scales = "free_x") +
  ggtext::geom_richtext(data = df_q1_estatisticas,
                        aes(label = estatistica),
                        color = "black",
                        fontface = "bold",
                        label.colour = "transparent",
                        fill = "transparent",
                        size = 7) +
  scale_fill_manual(values = c("green4", "orange3", "skyblue", "royalblue")) +
  scale_color_manual(values = "royalblue4") +
  labs(title = "t-crítico = 1.94, AIC = 156.95, pseudo-R² ajustado = 0.14") +
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

ggsave(filename = "grafico_pontos_q1.png", height = 10, width = 12)

## Diversidade beta -----

### Criando o modelo ----

modelo_beta <- glmmTMB::glmmTMB(Composição ~ `Abertura de dossel` +
                                  `Área de Poças` +
                                  `Altura da Serrapilheira` +
                                  `Distância dos corpos hidricos`,
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
                `Pr(>|z|)` = `Pr(>|z|)` |> round(5)) |>
  dplyr::relocate(Preditor, .before = Estimate) |>
  dplyr::filter(!Preditor |> stringr::str_detect("\\(")) |>
  dplyr::rename("β1" = Estimate,
                "EP" = `Std. Error`,
                "z" = `z value`,
                "p" = `Pr(>|z|)`) |>
  tidyr::unite(β1:EP,
               sep = " ± ",
               col = "β1 ± EP")

df_flexbeta_trat

#### Criando a tabela ----

flex_beta <- df_flexbeta_trat |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 1.5) |>
  flextable::add_footer_lines("z-crítico = 1.96, AIC = -91.2, pseudo-R² = 0.13") |>
  flextable::fontsize(size = 12, part = "all")

flex_beta

#### Exportando a tabela ----

flex_beta |>
  flextable::save_as_docx(path = "tabela_beta.docx")

### Gráfico ----

df_beta_estatisticas <- df_flexbeta_trat |>
  dplyr::mutate(`Valor Preditor` = c(0.03, 6, 3, 350),
                Composição = 0.45,
                df = 6,
                estatistica = paste0("β1 ± EP = ",
                                     `β1 ± EP`,
                                     "<br>z = ",
                                     z,
                                     "<sub>",
                                     df,
                                     "</sub>, p = ",
                                     p)) |>
  dplyr::select(-c(2:4, 7))

df_beta_estatisticas

df_beta |>
  tidyr::pivot_longer(cols = `Abertura de dossel`:`Distância dos corpos hidricos`,
                      names_to = "Preditor",
                      values_to = "Valor Preditor") |>
  dplyr::mutate(Preditor = Preditor |>
                  stringr::str_replace("hidrico", "hídrico")) |>
  ggplot(aes(`Valor Preditor`, Composição, fill = Preditor, color = Preditor)) +
  geom_point(shape = 21,
             color = "black",
             size = 3.5,
             stroke = 1) +
  geom_smooth(data = . %>%
                dplyr::filter(Preditor %in% c("Abertura de dossel",
                                              "Área de Poças")),
              method = "lm",
              se = FALSE) +
  facet_wrap(~Preditor, scales = "free_x") +
  ggtext::geom_richtext(data = df_beta_estatisticas,
                        aes(label = estatistica),
                        color = "black",
                        fontface = "bold",
                        label.colour = "transparent",
                        fill = "transparent",
                        size = 7) +
  scale_fill_manual(values = c("green4", "orange3", "skyblue", "royalblue")) +
  scale_color_manual(values = c("darkgreen", "royalblue4")) +
  scale_y_continuous(limits = c(0.1, 0.5)) +
  labs(x = "Distância preditora",
       y = "Distância de composição",
       title = "z-crítico = 1.96, AIC = -91.2, pseudo-R² ajustado = 0.20") +
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
