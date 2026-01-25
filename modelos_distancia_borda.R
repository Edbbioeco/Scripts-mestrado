# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

# Dados ----

## Dados de composição de espécies ----

## Dados de variáveis ambientais ----

# Diversidade alfa ----

# Diversidade beta e variação ambiental ----

# Modelos lineares ----

















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
