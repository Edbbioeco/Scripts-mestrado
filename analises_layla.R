# Pacotes ---- 

library(readxl)

library(tidyverse)

library(vegan)

library(terra)

library(tidyterra)

library(sf)

library(hillR)

library(sjPlot)

library(betapart)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("dados_layla.xlsx")

## Checando ----

dados %>% dplyr::glimpse()

# Curva do coletor ----

## criando uma base de dados para a análise ----

dados_trat <- dados %>%
  dplyr::select(2:4) %>%
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Abundância,
                     values_fill = 0) %>%
  dplyr::rename("rowname" = Ponto) %>%
  dplyr::mutate(rowname = paste0("Ponto ", rowname)) %>%
  tibble::column_to_rownames()

dados_trat

## Calculando ----

rare <- dados_trat %>%
  vegan::estaccumR(permutations = 1000) %>%
  summary(display = c("S", "chao"))

rare

## Criando o gráfico ----

gg_rare <- rare$S %>%
  as.data.frame() %>%
  dplyr::mutate(Tipo = "Observada") %>%
  dplyr::rename("Riqueza" = S) %>%
  dplyr::bind_rows(rare$chao %>%
                     as.data.frame() %>%
                     dplyr::mutate(Tipo = "Estimada") %>%
                     dplyr::rename("Riqueza" = Chao))

rownames(gg_rare) <- NULL

gg_rare

gg_rare %>%
  ggplot(aes(N, Riqueza, color = Tipo, fill = Tipo)) +
  geom_ribbon(aes(x = N, ymax = `97.5%`, ymin = `2.5%`), alpha = 0.3, linewidth = 0) +
  geom_line() +
  geom_point(shape = 21, color = "black", size = 2.5) +
  labs(x = "Unidades amostrais",
       fill = NULL,
       color = NULL) +
  scale_fill_manual(values = c("gold3", "cyan4")) +
  scale_color_manual(values = c("gold3", "cyan4")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.position = "bottom")

ggsave("rarefacao_layla.png", height = 10, width = 12)

# Dados ambientais ---- 

## Rasters ----

### Altitude ----

altitude <- terra::rast("raster_altitude_layla.tif")

altitude

ggplot() +
  tidyterra::geom_spatraster(data = altitude) +
  scale_fill_viridis_c(option = "turbo")

### EVI ----

evi <- terra::rast("evi_layla.tif")

evi

ggplot() +
  tidyterra::geom_spatraster(data = evi) +
  scale_fill_viridis_c(option = "turbo")

## Trasnformando os dados de coordenadas

coords <- dados %>%
  dplyr::summarise(Lon = Lon %>% mean,
                   Lat = Lat %>% mean,
                   .by = Ponto) %>%
  dplyr::select(c(Lon, Lat)) %>%
  sf::st_as_sf(coords = c("Lon", "Lat"))

coords %>%
  ggplot() +
  geom_sf()

## Extraindo os valores ----

### Altitude ----

alt_valores <- altitude %>%
  terra::extract(coords)

alt_valores

### EVI ----

evi_valores <- evi %>%
  terra::extract(coords)

evi_valores

# Regressão diversidade alfa ----

## Diversidade alfa ----

div_alfa <- dados_trat %>%
  hillR::hill_taxa(q = 1)

div_alfa

## Criando uma nova base de dados ----

dados_reg_alfa <- tibble::tibble(`Diversidade Alfa` = div_alfa,
                             Altitude = alt_valores$raster_altitude_layla,
                             EVI = evi_valores$`2024-09-06-00_00_2024-09-06-23_59_Sentinel-2_L2A_B05_(Raw)`)

dados_reg_alfa

## Modelo ----

### Testando multicolinearidade ----

cor.test(dados_reg_alfa$Altitude, dados_reg_alfa$EVI)

### Criando o modelo ----

modelo_alfa <- lm(`Diversidade Alfa` ~ Altitude + EVI, data = dados_reg_alfa)

### Avlaindo o modelo ----

modelo_alfa %>%
  sjPlot::plot_model(type = "diag") %>%
  sjPlot::plot_grid()

### Estatísticas do modelo ----

modelo_alfa %>%
  summary()

### F-Crítico ----

qf(p = 0.05, df1 = 2, df2 = 17, lower.tail = FALSE)

### Gráfico

dados_reg_alfa %>%
  tidyr::pivot_longer(cols = Altitude:EVI,
                      names_to = "Preditor",
                      values_to = "Valores Preditores") %>%
  ggplot(aes(`Valores Preditores`,`Diversidade Alfa`,  fill = Preditor)) +
  geom_point(shape = 21, color = "black", size = 2.5) +
  facet_wrap(~ Preditor,
             scales = "free_x",
             ncol = 1) +
  labs(fill = NULL) +
  scale_fill_manual(values = c("gold3", "green4")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.position = "bottom",
        strip.text = element_text(color = "black", size = 15),
        plot.margin = unit(c(0.1, 0.5, 0.1, 0.1), "cm"))


ggsave("reg_alfa.png", height = 10, width = 12)

# Regressão diversidade beta ----

## Calculando a diversidade beta ----

beta_total_valores <- dados_trat %>%
  betapart::beta.pair.abund() %>%
  .$beta.bray %>%
  as.numeric()

beta_total_valores

beta_bal_valores <- dados_trat %>%
  betapart::beta.pair.abund() %>%
  .$beta.bray.bal %>%
  as.numeric()

beta_bal_valores

beta_gra_valores <- dados_trat %>%
  betapart::beta.pair.abund() %>%
  .$beta.bray.gra %>%
  as.numeric()

beta_gra_valores

## Calculando a dissimilaridade das variáveis ambientais ----

### Altitude ----

alt_dis <- alt_valores %>%
  tibble::tibble() %>%
  vegan::vegdist(method = "euclidean") %>%
  as.numeric()

alt_dis

### EVI ----

evi_dis <- evi_valores %>%
  tibble::tibble() %>%
  vegan::vegdist(method = "euclidean") %>%
  as.numeric()

evi_dis

## Criando uma nova base de dados ----

dados_reg_beta <- tibble::tibble(`Dissimilaridade na Composição` = beta_total_valores,
                                 Altitude = alt_dis,
                                 EVI = evi_dis)

dados_reg_beta

## Modelo ----

### Testando multicolinearidade ----

cor.test(dados_reg_beta$Altitude, dados_reg_beta$EVI, method = "pearson")

### Modelo para altitude ----

#### Criando ----

modelo_beta_alt <- lm(`Dissimilaridade na Composição` ~ Altitude, data = dados_reg_beta)

#### Avaliando ----

modelo_beta_alt %>%
  sjPlot::plot_model(type = "diag") %>%
  sjPlot::plot_grid()

#### Estatísticas ----

modelo_beta_alt %>%
  summary()

#### F-Crítico ----

qf(p = 0.05, df1 = 1, df2 = 188, lower.tail = FALSE)

#### Gráfico ----

dados_reg_beta %>%
  ggplot(aes(Altitude, `Dissimilaridade na Composição`)) +
  geom_point(shape = 21, size = 2.5, color = "black", fill = "gold3") +
  geom_smooth(method = "lm", color = "orange4", se = FALSE) +
  labs(x = "Dissimilaridade de Altitude") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.position = "bottom",
        strip.text = element_text(color = "black", size = 15),
        plot.margin = unit(c(0.1, 0.5, 0.1, 0.1), "cm"))

ggsave("reg_beta_altitude.png", height = 10, width = 12)

### Modelo para EVI ----

#### Criando ----

modelo_beta_evi <- lm(`Dissimilaridade na Composição` ~ EVI, data = dados_reg_beta)

#### Avaliando ----

modelo_beta_evi %>%
  sjPlot::plot_model(type = "diag") %>%
  sjPlot::plot_grid()

#### Estatísticas ----

modelo_beta_evi %>%
  summary()

#### F-Crítico ----

qf(p = 0.05, df1 = 1, df2 = 188, lower.tail = FALSE)

#### Gráfico ----

dados_reg_beta %>%
  ggplot(aes(EVI, `Dissimilaridade na Composição`)) +
  geom_point(shape = 21, size = 2.5, color = "black", fill = "green3") +
  geom_smooth(method = "lm", color = "darkgreen", se = FALSE) +
  labs(x = "Dissimilaridade de EVI") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.position = "bottom",
        strip.text = element_text(color = "black", size = 15),
        plot.margin = unit(c(0.1, 0.5, 0.1, 0.1), "cm"))

ggsave("reg_beta_evi.png", height = 10, width = 12)

### Modelo Total ----

#### Criando ----

modelo_beta_total <- lm(`Dissimilaridade na Composição` ~ EVI + Altitude, data = dados_reg_beta)

#### Avaliando ----

modelo_beta_total %>%
  sjPlot::plot_model(type = "diag") %>%
  sjPlot::plot_grid()

#### Estatísticas ----

modelo_beta_total %>%
  summary()

### Gráfico ----

dados_reg_beta %>%
  tidyr::pivot_longer(cols = 2:3,
                      names_to = "Tipo",
                      values_to = "Dissimilaridade Ambiental") %>%
  ggplot(aes(`Dissimilaridade Ambiental`, `Dissimilaridade na Composição`, fill = Tipo, color = Tipo)) +
  geom_point(shape = 21, size = 2.5, color = "black") +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Tipo,
             scales = "free_x",
             ncol = 1) +
  labs(x = "Dissimilaridade de EVI",
       fill = NULL,
       color = NULL) +
  scale_fill_manual(values = c("gold3", "green4")) +
  scale_color_manual(values = c("gold4", "darkgreen")) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0.3, 1)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.position = "bottom",
        strip.text = element_text(color = "black", size = 15),
        plot.margin = unit(c(0.1, 0.5, 0.1, 0.1), "cm"))

ggsave("reg_beta.png", height = 10, width = 12)
