# PAcotes----

library(readxl)

library(tidyverse)

library(sf)

library(elevatr)

library(terra)

library(tidyterra)

library(magrittr)

library(writexl)

# Dados ----

## Aabertura de dossel e número de poças ----

### Importando -----

var1 <- readxl::read_xlsx("levantamento_variáveis_ambientais.xlsx")

### Visualizando -----

var1

var1 |> dplyr::glimpse()

## Altura da serrapilheira ----

### Importando -----

var2 <- readxl::read_xlsx("levantamento_variáveis_ambientais.xlsx",
                          sheet = 2)

### Visualizando -----

var2

var2 |> dplyr::glimpse()

## Área de poças d'água ----

### Importando -----

var3 <- readxl::read_xlsx("levantamento_variáveis_ambientais.xlsx",
                          sheet = 3)

### Visualizando -----

var3

var3 |> dplyr::glimpse()

## Saltinho ----

### Importando ----

saltinho <- sf::st_read("Saltinho.shp")

### Visualizando ----

ggplot() +
  geom_sf(data = saltinho)

## Borda da mata ----

### Importando ----

borda <- sf::st_read("borda_saltinho.shp")

### Visualizando ----

borda

ggplot() +
  geom_sf(data = borda, color = "black", linewidth = 1)

## Parcelas ----

### Importnado ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizando ----

parcelas

ggplot() +
  geom_sf(data = borda, color = "black", linewidth = 1) +
  geom_sf(data = parcelas, color = "black", linewidth = 1)

### Centróides ----

centroides <- parcelas |>
  sf::st_centroid()

centroides

ggplot() +
  geom_sf(data = borda, color = "black", linewidth = 1) +
  geom_sf(data = centroides, color = "black", linewidth = 1)

## Distância dos corpos d'água ----

### Importando ----

hid <- readxl::read_xlsx("dados_hidrico.xlsx")

### Visualizando ----

hid

hid |> dplyr::glimpse()

## Altitude ----

### Importando ----

alt <- elevatr::get_aws_terrain(locations = saltinho,
                                z = 14,
                                prj = 4674,
                                clip = "locations") |>
  terra::mask(saltinho) |>
  terra::crop(saltinho)

### Visualizando ----

alt

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  geom_sf(data = saltinho, fill = NA) +
  tidyterra::scale_fill_terrain_c()

## Dados de temperatura ----

### Importando ----

temp <- readxl::read_xlsx("levantamento_variáveis_ambientais.xlsx",
                          sheet = 6)

### Visualizando ----

temp

temp |> dplyr::glimpse()

# Dataframe de variáveis ambientais ----

## Extraindo valores de Altitude ----

alt_valores <- alt |>
  terra::extract(centroides)

alt_valores

## Extraindo os valores de distância da borda ----

borda_valores <- centroides |>
  sf::st_distance(borda |>
                    sf::st_boundary()) |>
  as.numeric()

borda_valores

## Função de cáçculo de área de poças ----

area_pocas <- function(comprimento, largura){

  area <- pi * (comprimento / 2) * (largura / 2)

  area <- area / 10000

  return(area)

}

area_pocas(comprimento = 4.5, largura = 3)

## Maiores valores das variáveis -----

pocas <- var3 |>
  dplyr::filter(`Unidade Amostral` != "T1P1") |>
  dplyr::summarise(area = area_pocas(comprimento = Comprimento,
                                     largura = Largura),
                   .by = c(`Unidade Amostral`, Campanha)) |>
  dplyr::mutate(area = dplyr::case_when(area |> is.na() ~ 0,
                                        .default = area)) |>
  dplyr::summarise(area = area |> sum(),
                   .by = c(`Unidade Amostral`, Campanha)) |>
  dplyr::summarise(area = area |> max(),
                   .by = `Unidade Amostral`)

pocas

dossel <- var1 |>
  dplyr::summarise(dossel = `Índice de abertura de dossel` |> mean(),
                   .by = c(`Unidade Amostral`, Campanha)) |>
  tidyr::drop_na() |>
  dplyr::summarise(dossel = dossel |> max(),
                   .by = `Unidade Amostral`)

dossel

numero_pocas <-  var1 |>
  dplyr::summarise(numero = `Número de poças` |> max(),
                   .by = c(`Unidade Amostral`, Campanha)) |>
  tidyr::drop_na() |>
  dplyr::summarise(numero = numero |> max(),
                   .by = `Unidade Amostral`)

numero_pocas

altura <- var2 |>
  dplyr::summarise(altura = Altura |> mean(),
                   .by = c(`Unidade Amostral`, Campanha)) |>
  tidyr::drop_na() |>
  dplyr::summarise(altura = altura |> max(),
                   .by = `Unidade Amostral`)

altura

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

## Dataframe dos valores ----

df_ambientais <- pocas |>
  dplyr::left_join(dossel,
                   by = "Unidade Amostral") |>
  dplyr::left_join(numero_pocas,
                   by = "Unidade Amostral") |>
  dplyr::left_join(altura,
                   by = "Unidade Amostral") |>
  dplyr::left_join(temp,
                   by = "Unidade Amostral") |>
  dplyr::left_join(hid[, 3:4],
                   by = "Unidade Amostral") |>
  dplyr::mutate(`Distância da Borda` = borda_valores[-1],
                Altitude = alt_valores[-1, 2]) |>
  dplyr::rename("Área das poças" = area,
                "Abertura do dossel" = dossel,
                "Número de poças" = numero,
                "Altura da serrapilheira" = altura,
                "Distância dos corpos hídricos" = Distância)

df_ambientais

df_ambientais |> dplyr::glimpse()

## Exportando ----

df_ambientais |>
  writexl::write_xlsx("matriz_ambientais.xlsx")
