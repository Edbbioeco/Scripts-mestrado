# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

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

## Criando o dataframe -----

df_temp <- especies |>
  dplyr::left_join(temp,
                   by = "Unidade Amostral")

df_temp

# Modelos lineares ----

## Loop para as 3 espécies -----

modelos_temp <- function(especie){

  paste0("modelo linear para a espécie: ", especie) |>
    crayon::green() |>
    message()

  modelo_linear <- lm(df_temp[[especie]] ~ Temperatura,
                      data = df_temp)

  performance_modelo <- modelo_linear |>
    performance::check_model(check = c("qq",
                                       "normality",
                                       "homogeneity"))

  performance_modelo |> print()

  sumario <- modelo_linear |> summary()

  sumario |> print()

}

purrr::walk(df_temp[, 3:5] |> names(), modelos_temp)

## Loop com GLM Poisson ----

modelos_temp_glm <- function(especie){

  paste0("modelo linear para a espécie: ", especie) |>
    crayon::green() |>
    message()

  modelo_linear <- glm(df_temp[[especie]] ~ Temperatura,
                       data = df_temp,
                       family = poisson(link = "log"))

  performance_modelo <- modelo_linear |>
    DHARMa::simulateResiduals(plot = TRUE)

  performance_modelo |> print()

  sumario <- modelo_linear |> summary()

  sumario |> print()

}

purrr::walk(df_temp[, 3:5] |> names(), modelos_temp_glm)

## Gráfico ----

df_temp |>
  tidyr::pivot_longer(cols = 2:4,
                      names_to = "Espécie",
                      values_to = "Abundância") |>
  ggplot(aes(Temperatura, Abundância)) +
  geom_point(size = 5) +
  geom_smooth(data = . %>%
                dplyr::filter(Espécie == "Pristimantis ramagii"),
              se = FALSE,
              method = "lm") +
  facet_wrap(~Espécie, scales = "free_y", ncol = 2) +
  ggview::canvas(height = 10, width = 12)
