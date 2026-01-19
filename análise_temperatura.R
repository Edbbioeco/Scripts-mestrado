# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(performance)

# Dados ----

## Dados de abundância de espécies ----

### Importando ----

especies <- readxl::read_xlsx("levantamento_anuros.xlsx")

## Visualizando ----

especies

especies %>% dplyr::glimpse()

## Dados de temperatura ----

### Importando ----

var <- readxl::read_xlsx("levantamento_variáveis_ambientais.xlsx",
                         sheet = 5)

### Visualizando ----

var

var |> dplyr::glimpse()

# Tratamento dos dados ----

## Diversidade de espécies ----

### Criando a matriz ----

dados_alfa <- especies |>
  dplyr::filter(Ordem == "Anura" &
                  Epípeto != "natalensis" &
                  Gênero != "Frostius" &
                  Família != "Hylidae") |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(`Unidade Amostral`, Espécie, Campanha)) |>
  dplyr::summarise(Abundância = Abundância |> max(),
                   .by = c(`Unidade Amostral`, Espécie)) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Abundância,
                     values_fill = 0)

dados_alfa

nomes_linhas <- dados_alfa |>
  dplyr::pull(`Unidade Amostral`)

nomes_linhas

dados_alfa <- dados_alfa |>
  dplyr::select(-`Unidade Amostral`)

rownames(dados_alfa) <- nomes_linhas

dados_alfa |> names()

dados_alfa

### Valores de diversidade ----

diversidade <- dados_alfa |>
  vegan::renyi(scales = c(0, 1),
               hill = TRUE) |>
  tibble::as_tibble() |>
  rename("Q0" = `0`,
         "Q1" = `1`) |>
  dplyr::mutate(`Unidade Amostral` = especies |>
                  dplyr::filter(`Unidade Amostral` != "T1P1") |>
                  dplyr::pull(`Unidade Amostral`) |>
                  unique())

diversidade

## Dados de temperatura ----

temp <- var |>
  dplyr::select(`Unidade Amostral`, Campanha,
                dplyr::contains("Temperatura")) |>
  tidyr::pivot_longer(cols = dplyr::contains("Temperatura"),
                      names_to = "tipo de temperatura",
                      values_to = "Temperatura") |>
  dplyr::summarise(Temperatura = Temperatura |> mean(),
                   .by = c(`Unidade Amostral`, Campanha)) |>
  dplyr::slice_max(Temperatura,
                   by = `Unidade Amostral`) |>
  tidyr::drop_na()

temp

## Unindo os dados ----

dados_unidos <- diversidade |>
  dplyr::left_join(temp,
                   by = "Unidade Amostral")

dados_unidos

# Setando o tema dos gráficos ----

source("C:/Users/LENOVO/OneDrive/Documentos/Funções/tema.R")

# Modelos lineares ----

## Q = 0 ----

### Criando o modelo ----

modelo_q0 <- glm(Q0 ~ Temperatura,
                 data = dados_unidos,
                 family = poisson(link = "log"))

### Avaliando o modelo ----

par(mfrow = c(2, 2)); modelo_q0 %>%plot(); par(mfrow = c(1, 1))

### Estatísticas do modelo ----

modelo_q0 |>
  summary()

### Gráfico ----

dados_unidos |>
  ggplot(aes(Temperatura, Q0)) +
  geom_point(shape = 21, size = 5, color = "black", fill = "orange", stroke = 1)

## Q = 1 ----

### Criando o modelo ----

modelo_q1 <- lm(Q1 ~ Temperatura,
                data = dados_unidos)

### Avaliando o modelo ----

modelo_q1 %>% performance::check_model(check = c("vif",
                                                 "qq",
                                                 "normality",
                                                 "homogeneity"))

### Estatísticas do modelo ----

modelo_q1 |>
  summary()

### Gráfico ----

dados_unidos |>
  ggplot(aes(Temperatura, Q1)) +
  geom_point(shape = 21, size = 5, color = "black", fill = "orange", stroke = 1)
