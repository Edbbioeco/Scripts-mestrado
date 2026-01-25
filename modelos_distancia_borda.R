# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(performance)

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

## Dataframe das estatísticas do modelo ----

## Gráfico ----

# Modelos lineares de diversidade beta ----

## Criando o modelo ----

## Pressupostos do modelo ----

## Estatísticas o modelo ----

## Dataframe das estatísticas do modelo ----

## Gráfico ----

# Ordenação das espécies ----
