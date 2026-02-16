# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(vegan)

library(ggview)

library(flextable)

library(betapart)

library(reshape2)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("levantamento_anuros.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

## Tratando ----

dados %<>%
  dplyr::mutate(`Segmento da parcelas` = `Segmento da parcelas` |>
                  as.character())

dados |> dplyr::glimpse()

# Diversidade alfa ----

## Criando a matriz ----

dados_alfa <- dados |>
  dplyr::filter(Ordem == "Anura" &
                  !Epípeto %in% c("natalensis", "mystaceus") &
                  Gênero != "Frostius" &
                  Família != "Hylidae") |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(`Unidade Amostral`, Espécie, Campanha)) |>
  dplyr::summarise(Abundância = Abundância |> max(),
                   .by = c(`Unidade Amostral`, Espécie)) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Abundância,
                     values_fill = 0) |>
  tibble::column_to_rownames("Unidade Amostral")

dados_alfa

## ìndices de diversidade ----

div_alfa <- dados_alfa |>
  vegan::renyi(scales = 0:1, hill = TRUE)

div_alfa

## Gráfico ----

div_alfa |>
  dplyr::mutate(Parcela = dados_alfa |> rownames()) |>
  dplyr::rename("q = 0" = `0`,
                "q = 1" = `1`) |>
  tidyr::pivot_longer(cols = dplyr::contains("q"),
                      values_to = "Diversidade",
                      names_to = "Tipo")  |>
  ggplot(aes(Parcela, Diversidade)) +
  geom_col(col = "black", fill =  "gold") +
  facet_wrap(~ Tipo) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        panel.border = element_rect(color = "black"),
        legend.position = "bottom",
        panel.background = element_rect(color = "black", linewidth = 1))

ggsave(filename = "grafico_diversidades.png", height = 10, width = 12)

## Média e desvio padrão dos valores de diversidade ----

div_alfa |>
  dplyr::rename("q = 0" = `0`,
                "q = 1" = `1`) |>
  dplyr::pull(`q = 1`) |>
  mean()

div_alfa |>
  dplyr::rename("q = 0" = `0`,
                "q = 1" = `1`) |>
  dplyr::pull(`q = 1`) |>
  sd()

## Comunidades mais diversas ----

div_alfa |>
  dplyr::mutate(`Unidade Amostral` = dados_alfa |> rownames()) |>
  dplyr::rename("q = 0" = `0`,
                "q = 1" = `1`) |>
  dplyr::summarise(Diversidade = (`q = 0` - `q = 1`) / `q = 0`,
                   .by = `Unidade Amostral`) |>
  dplyr::arrange(Diversidade)

## Tabela ----

### Tratando os dados ----

df_flex <- div_alfa |>
  dplyr::mutate(`Unidade Amostral` = dados_alfa |> rownames()) |>
  dplyr::rename("Riqueza" = `0`,
                "Diversidade (Q = 1)" = `1`) |>
  dplyr::select(`Unidade Amostral`, Riqueza, `Diversidade (Q = 1)`)|>
  dplyr::mutate(`Diversidade (Q = 1)` = `Diversidade (Q = 1)` |> round(2)) |>
  dplyr::arrange(`Unidade Amostral`)

df_flex

### Criando a tabela -----

tabela_flex <- df_flex |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 1.5)

tabela_flex

### Exportando -----

tabela_flex |>
  flextable::save_as_docx(path = "tabela_valores_diversidades.docx")

# Curva de rarefação ----

## Calcular a curva ----

curva <- dados_alfa |>
  vegan::estaccumR() |>
  summary(display = c("S", "chao"))

curva

dados_curva <- curva$S |>
  tibble::as_tibble() |>
  dplyr::rename("Richness" = S,
                "Number of sampling units" = N) |>
  dplyr::mutate(Tipo = "Observed") |>
  dplyr::bind_rows(curva$chao |>
                     tibble::as_tibble() |>
                     dplyr::rename("Richness" = Chao,
                                   "Number of sampling units" = N) |>
                     dplyr::mutate(Tipo = "Estimated"))

dados_curva |> as.data.frame()

## Gráfico ----

dados_curva |>
  ggplot(aes(`Number of sampling units`, Richness, color = Tipo, fill = Tipo)) +
  geom_ribbon(aes(x = `Number of sampling units`,
                  ymin = Richness - Std.Dev,
                  ymax = Richness + Std.Dev),
              alpha = 0.3,
              color = "transparent") +
  geom_line(linewidth = 1) +
  geom_point(color = "black", size = 5, shape = 21) +
  scale_x_continuous(breaks = seq(1, 11, 1),
                     limits = c(1, 11)) +
  scale_y_continuous(breaks = seq(1, 14, 1),
                     limits = c(1, 14)) +
  scale_color_manual(values = c("royalblue", "orange")) +
  scale_fill_manual(values = c("royalblue", "orange")) +
  labs(fill = NULL,
       color = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 25),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 19),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.text = element_text(color = "black", size = 20),
        legend.position = "bottom",
        title = element_text(color = "black", size = 25),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "curva.png", height = 10, width = 12)

# Diversidade beta ----

## Valores de diversidade beta ----

indices_beta <- dados_alfa |>
  betapart::beta.multi.abund()

indices_beta

## Matriz de Bray-Curtis ----

matriz_beta <- dados_alfa |>
  beta.pair.abund()

matriz_beta

## Gráfio ----

### Separando por tipo de componente ----

separar_comp <- function(id, nome){

  matriz_beta_trat <- matriz_beta[[id]] |>
    as.matrix()

  matriz_beta_trat[upper.tri(matriz_beta_trat)] <- NA

  matriz_beta_trat_df <- matriz_beta_trat |>
    as.matrix() |>
    reshape2::melt() |>
    tibble::as_tibble() |>
    dplyr::mutate(Igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                           .default = "Não"),
                  value = value |> round(2),
                  indice = paste0(nome,
                                  ": ",
                                  indices_beta[[id]] |> round(2))) |>
    dplyr::filter(Igual == "Não") |>
    dplyr::select(-Igual) |>
    tidyr::drop_na()

  assign(paste0("beta_df_", nome |>
                  stringr::str_replace(" ",
                                       "_")),
         matriz_beta_trat_df,
         envir = globalenv())

}

purrr::walk2(1:3,
             c("Variação Balanciada",
               "Gradiente de Variação",
               "Bray-Curtis"),
             separar_comp)

beta_df_gg <- ls(pattern = "beta_df_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(indice = indice |>
                  forcats::fct_relevel(c("Bray-Curtis: 0.69",
                                         "Variação Balanciada: 0.43",
                                         "Gradiente de Variação: 0.26")))

beta_df_gg

### Gráfico ----

beta_df_gg |>
  ggplot(aes(Var1, Var2, fill = value, label = value)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(color = "black", size = 3.5, fontface = "bold") +
  coord_equal() +
  scale_fill_viridis_c(limits = c(0, 1)) +
  labs(x = NULL,
       y = NULL,
       fill = "Dissimilaridade de Bray-Curtis") +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 20,
                               frame.colour = "black",
                               frame.linewidth = 1,
                               ticks.colour = "black",
                               ticks.linewidth = 1)) +
  facet_wrap(~indice) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15, angle = 90, hjust = 1),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 15),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.text = element_text(color = "black", size = 15),
        legend.position = "bottom",
        title = element_text(color = "black", size = 15),
        panel.background = element_rect(color = "black", linewidth = 1))

ggsave(filename = "grafico_calor_beta.png", height = 12, width = 12)

## nMDS ----

### Calculando ----

nmds <- matriz_beta[[3]] |>
  vegan::metaMDS(distance = "bray", k = 2)

nmds

## Espécies mais importantes ----

### Calculo das espécies mais relevantes ----

sps_infleuncia <- vegan::envfit(nmds, dados_alfa, permutations = 999)

sps_infleuncia

### Cálculo de estatística F ----

F_calc <- ((sps_infleuncia$vectors$r / 2) /
             ((1 - sps_infleuncia$vectors$r) / (11 - 2 - 1)))

F_calc

F_crit <- qf(p = 0.05, df1 = 1, df2 = 9, lower.tail = FALSE)

F_crit

### Dataframe das informações ----

df_envfit <- sps_infleuncia$vectors$arrows |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::select(-c(2:3)) |>
  dplyr::rename("Espécie" = rowname) |>
  dplyr::mutate(`F` = F_calc |> round(2),
                GL = "1, 9",
                p = sps_infleuncia$vectors$pvals,
                `R²` = sps_infleuncia$vectors$r |> round(2))

df_envfit

### Tabwela flextable ----

df_envfit_flex <- df_envfit |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 2, j = 1) |>
  flextable::width(width = 0.75, j = 2:5) |>
  flextable::italic(j = 1, part = "body")|>
  flextable::bg(i = ~`F` >= 5.11, bg = "gray") |>
  flextable::bg(i = ~p < 0.05, bg = "gray") |>
  flextable::set_caption(caption = "F-Crítico = 5.11")

df_envfit_flex

### Exportando tabela flextable ----

df_envfit_flex |>
  flextable::save_as_docx(path = "tabela_influencia_especies.docx")

### Gráfico ----

dados_nmds <- nmds |>
  vegan::scores() |>
  tibble::as_tibble() |>
  dplyr::mutate(`Unidade Amostral` = dados_alfa |> rownames()) |>
  dplyr::left_join(dados |>
                     dplyr::select(2, 4) |>
                     dplyr::distinct(`Unidade Amostral`,
                                     .keep_all = TRUE),
                   by = "Unidade Amostral")

dados_nmds

centroides <- dados_nmds |>
  dplyr::summarise(c_x = NMDS1 |> mean(),
                   c_y = NMDS2 |> mean(),
                   .by = Trilha)

centroides

dados_nmds <- dados_nmds |>
  dplyr::left_join(centroides,
                   by = "Trilha")

dados_nmds

dados_nmds |>
  ggplot(aes(NMDS1, NMDS2,
             color = Trilha,
             label = `Unidade Amostral`)) +
  geom_text(fontface = "bold",
            size = 7,
            show.legend = FALSE) +
  scale_color_manual(values = c("orangered",
                                "darkgreen",
                                "purple",
                                "royalblue")) +
  labs(color = "Trilha") +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5),
         color = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  labs(title = "k = 0.073") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        panel.border = element_rect(color = "black"),
        legend.position = "bottom",
        title = element_text(color = "black", size = 15),
        panel.background = element_rect(color = "black", linewidth = 1))

ggsave(filename = "nmds.png", height = 10, width = 12)
