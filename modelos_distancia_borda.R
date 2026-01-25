# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(performance)

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

# Diversidade alfa ----

## Calculando a diversidade ---

especies

## Gerando o dataframe para o modelo linear ----

# Diversidade beta e variação ambiental ----

## Calculando a diversidade beta ----

## Calculando a variação ambiental ----

## Gerando o dataframe para o modelo linear ----

# Modelos lineares de diversidade alfa ----

## Criando o modelo ----

## Avaliando o modelo ----

## Dataframe das estatísticas do modelo ----

## Gráfico ----

# Modelos lineares de diversidade beta ----

## Criando o modelo ----

## Avaliando o modelo ----

## Dataframe das estatísticas do modelo ----

## Gráfico ----

















div_alfa2 <- dados_alfa |>
  vegan::renyi(scales = 0:2, hill = TRUE) |>
  dplyr::mutate(`Unidade Amostral` =  dados_alfa |> rownames(),
                Eq = `2` / `1`)

div_alfa2

ambientais2 <- ambientais |>
  dplyr::left_join(div_alfa2,
                   by = "Unidade Amostral")

ambientais2 |> as.data.frame()

lm(`1` ~ `Distância da Borda`, data = ambientais2) |>
  summary()

lm(`1` ~ `Distância da Borda`, data = ambientais2) |>
  performance::check_model(check = c("homogeneity",
                                     "qq",
                                     "normality"))

ambientais2 |>
  ggplot(aes(`Distância da Borda`, `1`)) +
  geom_point() +
  geom_smooth(method = "lm")

dados_alfa

dados_alfa |>
  dplyr::mutate(`Distância da Borda` = ambientais2$`Distância da Borda`) |>
  ordenaR::order_circle(gradient = "Distância da Borda",
                        species = 1:10)
