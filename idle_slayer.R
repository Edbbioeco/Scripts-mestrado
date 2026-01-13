# Pacotes ----

library(readxl)

library(tidyverse)

library(sjPlot)

library(vegan)

library(FactoMineR)

library(ggvegan)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("idle_slayer_baus.xlsx")

## Visualizando ----

dados

dados %>% dplyr::glimpse()

## Tratando ----

dados_trat <- dados %>% tidyr::drop_na()

dados_trat

# Análises ----

## Correlação ----

dados_trat %>%
  dplyr::select_if(is.numeric) %>%
  cor()

## Modelos ANOVA ----

### Criando a função ----

modelos_anova <- function(x){

  anova <- aov(dados_trat[[x]] ~ Estratégia, data = dados_trat) %>%
    summary()

  stringr::str_glue("modelo para {x}") %>% message()

  print(anova)

}

### ANOVAs ----

purrr::walk(dados_trat[2:5] %>% names(), modelos_anova)

## Gráfico ----

dados_trat %>%
  tidyr::pivot_longer(cols = 2:5,
                      names_to = "Variavel",
                      values_to = "Valor") %>%
  ggplot(aes(Estratégia, Valor, fill = Variavel)) +
  geom_jitter(width = 0.01, shape = 21, color = "black", size = 5) +
  facet_wrap(~ Variavel, scales = "free_y")

## Permanova ----

### Matriz de distancia ----

dist <- dados_trat %>%
  dplyr::select_if(is.numeric) %>%
  vegan::decostand(method = "standardize") %>%
  vegan::vegdist(method = "euclidean")

dist

### Permanova ----

vegan::adonis2(dist ~ Estratégia, data = dados_trat)

### PCA

pca <- dados_trat %>%
  dplyr::select_if(is.numeric) %>%
  vegan::decostand(method = "standardize") %>%
  FactoMineR::PCA()

pca

pca$ind$coord %>%
  as.data.frame() %>%
  dplyr::mutate(Estratégia = dados_trat$Estratégia) %>%
  ggplot(aes(Dim.1, Dim.2, fill = Estratégia)) +
  geom_point(shape = 21, color = "black", size = 5) +
  scale_fill_viridis_d()
