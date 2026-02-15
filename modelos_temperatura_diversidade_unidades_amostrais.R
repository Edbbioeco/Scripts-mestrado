# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(vegan)

library(lme4)

library(performance)

library(DHARMa)

# Dados ----

## Dados de abundância ----

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

## Dados de temperatura ----

### Importando ----

temp <- readxl::read_xlsx("levantamento_variáveis_ambientais.xlsx",
                          sheet = 6)

### Visualizando ----

temp

temp |> dplyr::glimpse()

# Dataframe dos dados ----

## Tratando dados de espécies ----

especies %<>%
  dplyr::filter(Epípeto %in% c("ramagii", "hylaedactyla", "hoogmoedi")) %<>%
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(`Unidade Amostral`, Espécie, Campanha)) %<>%
  dplyr::summarise(Abundância = Abundância |> max(),
                   .by = c(`Unidade Amostral`, Espécie)) %<>%
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Abundância,
                     values_fill = 0)

especies

especies |> dplyr::glimpse()

## Tratando dados de temperatura ----

temp %<>%
  dplyr::filter(`Unidade Amostral` != "T1P1") %<>%
  tidyr::pivot_longer(cols = dplyr::contains("Temperatura"),
                      names_to = "tipo de temperatura",
                      values_to = "temperatura") %<>%
  dplyr::summarise(Temperatura = temperatura |> mean(),
                   .by = c(`Unidade Amostral`, Campanha)) %<>%
  tidyr::drop_na() %<>%
  dplyr::summarise(Temperatura = Temperatura |> mean(),
                   .by = `Unidade Amostral`)

temp

temp |> dplyr::glimpse()

## Calculando a diversidade alfa ----

div_alfa <- especies |>
  dplyr::select(dplyr::where(is.numeric)) |>
  vegan::renyi(hill = TRUE, scales = 1)

div_alfa

## Calculando a diversidade Beta ----

div_beta <- especies |>
  dplyr::select(dplyr::where(is.numeric)) |>
  vegan::vegdist() |>
  as.numeric()

div_beta

## Calculando a distância de temperatura entre as uunidades amostrais ----

dist_temp <- temp |>
  dplyr::select(dplyr::where(is.numeric)) |>
  vegan::vegdist(method = "euclidean") |>
  as.numeric()

dist_temp

## Criando o dataframe -----

### Diversidade alfa ----

alfa_df <- temp |>
  dplyr::mutate(`Q = 1` = div_alfa)

alfa_df

### Diversidade beta ----

beta_df <- tibble::tibble(Composicao = div_beta,
                          Temperatura = dist_temp)

beta_df

# Modelos lineares ----

## Diversidade alfa ----

## Diversidade beta ----

# Gráficos ----

## Diversidade alfa ----

alfa_df |>
  ggplot(aes(Temperatura, `Q = 1`)) +
  geom_point()

## Diversidade beta ----

beta_df |>
  ggplot(aes(Temperatura, Composicao)) +
  geom_point()
