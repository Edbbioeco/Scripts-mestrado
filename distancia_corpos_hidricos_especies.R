# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(terra)

library(tidyterra)

library(vegan)

library(DHARMa)

library(performance)

library(writexl)

# Setando o tema dos gráficos ----

source("C:/Users/LENOVO/OneDrive/Documentos/Funções/tema.R")

# Dados ----

### Importando ----

especies <- readxl::read_xlsx("levantamento_anuros.xlsx")

## Visualizando ----

especies

especies |> dplyr::glimpse()

## Saltinho ----

### Importando ----

saltinho <- sf::st_read("Saltinho.shp")

### Visualizando ----

saltinho

ggplot() +
  geom_sf(data = saltinho, color = "black", linewidth = 1)

## Parcelas ----

### Importnado ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizando ----

parcelas

ggplot() +
  geom_sf(data = saltinho, color = "black", linewidth = 1) +
  geom_sf(data = parcelas, color = "black", linewidth = 1)

## Corpos hídricos ----

### Importando ----

hidrico <- sf::st_read("corpos_hidricos_saltinho.gpkg")

### Visualizando ----

hidrico

ggplot() +
  geom_sf(data = saltinho, color = "black", linewidth = 1) +
  geom_sf(data = hidrico, color = "blue", fill = "blue",
          alpha = 0.5, linewidth = 1) +
  geom_sf(data = parcelas, color = "black", linewidth = 1)

## Raster de altitude ----

### Importando ----

alt <- terra::rast("altitude.tif")

### Visualizando ----

alt

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  tidyterra::scale_fill_whitebox_c(palette = "arid", direction = -1,
                                   name = "Altitude",
                                   guide = guide_colorbar(order = 1,
                                                          title.position = "top",
                                                          title.hjust = 0.5,
                                                          barwidth = 15,
                                                          frame.colour = "black",
                                                          frame.linewidth = 1,
                                                          ticks.colour = "black",
                                                          ticks.linewidth = 1)) +
  scale_x_continuous(limits = c(-35.2, -35.164),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(-8.74, -8.711),
                     expand = c(0, 0)) +
  geom_sf(data = saltinho, color = "black", linewidth = 1, fill = "transparent") +
  geom_sf(data = hidrico, color = "blue", fill = "transparent", linewidth = 1) +
  geom_sf(data = parcelas, color = "black", linewidth = 1)

### Exportando ----

# Extraindo os valores ----

## Diversidade de espécies

### Criando a matriz ----

dados_alfa <- especies |>
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
                     values_fill = 0)

dados_alfa

dados_alfa_trat <- dados_alfa |>
  tibble::column_to_rownames(var = "Unidade Amostral")

dados_alfa_trat

### Valores de diversidade ----

diversidade <- dados_alfa_trat |>
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

## Distância das parcelas ----

### Borda ----

borda_hidrico <- hidrico[c(1:2), ] |>
  sf::st_boundary()

borda_hidrico

### Distância horizontal ----

#### Bordas do açude ----

dist_acude <- sf::st_distance(parcelas |>
                                dplyr::filter(Trlh.Pr != "1-1"),
                              borda_hidrico) |>
  tibble::as_tibble() |>
  dplyr::mutate(`Unidade Amostral` = nomes_linhas) |>
  tidyr::pivot_longer(cols = dplyr::contains("V"),
                      values_to = "Distância",
                      names_to = "Tipo") |>
  dplyr::summarise(Distância = Distância |> min(),
                   .by = `Unidade Amostral`) |>
  dplyr::mutate(Distância = Distância |> as.numeric(),
                tipo = "açude")

dist_acude

#### Rios -----

dist_rios <- sf::st_distance(parcelas |>
                               dplyr::filter(Trlh.Pr != "1-1"),
                             hidrico[c(3:5), ]) |>
  tibble::as_tibble() |>
  dplyr::mutate(`Unidade Amostral` = nomes_linhas) |>
  tidyr::pivot_longer(cols = dplyr::contains("V"),
                      values_to = "Distância",
                      names_to = "Tipo") |>
  dplyr::summarise(Distância = Distância |> min(),
                   .by = `Unidade Amostral`) |>
  dplyr::mutate(Distância = Distância |> as.numeric(),
                tipo = "rios")

dist_rios

#### Unindo os dados ----

delt_dist <- dplyr::bind_rows(dist_acude, dist_rios) |>
  dplyr::summarise(Distância = Distância |> min(),
                   .by = `Unidade Amostral`) |>
  dplyr::mutate(Distância = Distância |> as.numeric()) |>
  dplyr::left_join(dplyr::bind_rows(dist_acude, dist_rios),
                   by = c("Unidade Amostral", "Distância"))

delt_dist

### Distância vertical ----

#### Coordenadas das menores distâncias ----

corpo_proximo_acude <- sf::st_nearest_feature(parcelas |>
                                                dplyr::filter(Trlh.Pr != "1-1"),
                                              borda_hidrico)

corpo_proximo_acude

corpo_proximo_rios <- sf::st_nearest_feature(parcelas |>
                                               dplyr::filter(Trlh.Pr != "1-1"),
                                             hidrico[c(3:5), ])

corpo_proximo_rios

corpo_proximo <- c(4, 2, 2, 4, 4, 4, 1, 4, 5, 5, 4)

corpo_proximo

linhas_conexao_acude <- sf::st_nearest_points(parcelas |>
                                                dplyr::filter(Trlh.Pr != "1-1"),
                                              borda_hidrico)

linhas_conexao_acude <- linhas_conexao_acude[c(4, 6, 13), ] |>
  sf::st_as_sf()

linhas_conexao_acude

linhas_conexao_rios <- sf::st_nearest_points(parcelas |>
                                               dplyr::filter(Trlh.Pr != "1-1"),
                                             hidrico[c(3:5), ])

linhas_conexao_rios <- linhas_conexao_rios[c(2, 11, 14, 17, 23, 27, 30, 32), ] |>
  sf::st_as_sf()

linhas_conexao_rios

linhas_conexao <- dplyr::bind_rows(linhas_conexao_acude, linhas_conexao_rios) |>
  dplyr::mutate(`Unidade Amostral` = nomes_linhas) |>
  sf::st_cast("LINESTRING")

linhas_conexao

string <- hidrico$Name[corpo_proximo]

string

tbl <- tibble::tibble(`Unidade Amostral` = nomes_linhas,
                      string)

tbl

shp_pontos <- linhas_conexao |>
  dplyr::left_join(tbl,
                   by = "Unidade Amostral") |>
  sf::st_coordinates() |>
  as.data.frame() |>
  dplyr::slice_tail(n = 1,
                    by = L1) |>
  sf::st_as_sf(coords = c("X", "Y"),
               crs = 4674)

shp_pontos

ggplot() +
  geom_sf(data = saltinho, color = "black", linewidth = 1) +
  geom_sf(data = hidrico, color = "blue", fill = "blue",
          alpha = 0.5, linewidth = 1) +
  geom_sf(data = parcelas, color = "black", linewidth = 1) +
  geom_sf(data = linhas_conexao, color = "red", linewidth = 1) +
  geom_sf(data = shp_pontos, color = "green4", size = 5)

### Extraindo os valores de altitude ----

#### Centroides ----

alt_cent <- alt |>
  terra::extract(parcelas |>
                   dplyr::filter(Trlh.Pr != "1-1")) |>
  dplyr::summarise(Altitude = file7114380c687d |> mean(),
                   .by = ID) |>
  dplyr::pull(Altitude)

alt_cent

#### Pontos mais proximo ----

alt_pontos <- alt |>
  terra::extract(shp_pontos) |>
  dplyr::pull(file7114380c687d)

alt_pontos

#### Diferença de altura ----

delt_alt <- alt_cent - alt_pontos

delt_alt

### Distancia real ----

distancia_real <- sqrt(delt_dist$Distância^2 + delt_alt^2)

distancia_real

## Unindo os dados ----

dados_hidrico_div <- diversidade |>
  dplyr::mutate(Distância = distancia_real)

dados_hidrico_div

# Modelos lineares ----

## Q = 0 ----

### Criando o modelo ----

modelo_q0 <- glm(Q0 ~ Distância,
                 data = dados_hidrico_div,
                 family = poisson(link = "log"))

### Avaliando o modelo ----

modelo_q0 |> DHARMa::simulateResiduals(plot = TRUE)

### Estatísticas do modelo ----

modelo_q0 |>
  summary()

### Gráfico ----

dados_hidrico_div |>
  ggplot(aes(Distância, Q0)) +
  geom_point(shape = 21, size = 5, color = "black", fill = "orange", stroke = 1)

## Q = 1 ----

### Criando o modelo ----

modelo_q1 <- lm(Q1 ~ Distância,
                data = dados_hidrico_div)

### Avaliando o modelo ----

modelo_q1 %>% performance::check_model(check = c("vif",
                                                 "qq",
                                                 "normality",
                                                 "homogeneity"))

### Estatísticas do modelo ----

modelo_q1 |>
  summary()

### Gráfico ----

dados_hidrico_div |>
  ggplot(aes(Distância, Q1)) +
  geom_point(shape = 21, size = 5, color = "black", fill = "orange", stroke = 1)

## Exportando os dados ----

dados_hidrico_div |>
  writexl::write_xlsx("dados_hidrico.xlsx")
