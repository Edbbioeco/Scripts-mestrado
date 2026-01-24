# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(elevatr)

library(terra)

library(tidyterra)

library(DHARMa)

# Dados ----

## Espécies -----

### Importando ----

especies <- readxl::read_xlsx("levantamento_anuros.xlsx")

## Visualizando ----

especies

especies %>% dplyr::glimpse()

## Tratando ----

especies <- especies %>%
  dplyr::mutate(`Segmento da parcelas` = `Segmento da parcelas` %>%
                  as.character())

especies %>% dplyr::glimpse()

## Variáveis ambientais ----

### Densidade de Jaqueiras, abertura de dossel e número de poças ----

#### Importando -----

var1 <- readxl::read_xlsx("levantamento_variáveis_ambientais.xlsx")

#### Visualizando -----

var1

var1 %>% dplyr::glimpse()

### Altura da serrapilheira ----

#### Importando -----

var2 <- readxl::read_xlsx("levantamento_variáveis_ambientais.xlsx",
                          sheet = 2)

#### Visualizando -----

var2

var2 %>% dplyr::glimpse()

## Coordenadas nas parcelas ----

### Importnado ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizando ----

parcelas

ggplot() +
  geom_sf(data = parcelas)

#### Extraindo as coordenadas ----

parcelas_trat <- parcelas %>%
  sf::st_coordinates() %>%
  as.data.frame() %>%
  dplyr::mutate(Segmento = rep(seq(0, 250, 10),
                               times = 12),
                Parcela = c(rep(paste0("T1-P", 1:4), each = 26),
                            rep(paste0("T2-P", 1:4), each = 26),
                            rep(paste0("T3-P", 1:2), each = 26),
                            rep(paste0("R", 1:2), each = 26))) %>%
  dplyr::filter(Segmento %in% seq(0, 250, 50)) %>%
  sf::st_as_sf(coords = c("X", "Y"),
               crs = 4674)

parcelas_trat

ggplot() +
  geom_sf(data = parcelas_trat)

## Inclinação ----

### Shapefile de Saltinho ----

saltinho <- sf::st_read("saltinho.shp")

### Importando ----

inc <- elevatr::get_elev_raster(locations = saltinho,
                                clip = "locations",
                                z = 14) %>%
  terra::rast() %>%
  terra::terrain(neighbors = 4)

inc

ggplot() +
  tidyterra::geom_spatraster(data = inc) +
  geom_sf(data = saltinho, color = "red", fill = "transparent") +
  scale_fill_viridis_c(na.value = "transparent")

inc %>% terra::writeRaster("inc.tif", overwrite = TRUE)

### Extraindo os valores ----

inc <- terra::rast("inc.tif")

inc_valores <- inc %>%
  terra::extract(parcelas_trat)

inc_valores

inc_valores[inc_valores %>% is.na()] <- 0

inc_valores

# Dataframe do modelo -----

## Riqueza e diversidade alfa -----

### Criando a matriz ----

especies_alfa <- especies %>%
  dplyr::filter(Gênero == "Pristimantis") %>%
  dplyr::summarise(Abundância = Abundância %>% sum(),
                   .by = c(`Unidade Amostral`, Campanha)) %>%
  dplyr::summarise(Abundância = Abundância %>% max(),
                   .by = `Unidade Amostral`)

especies_alfa

### Valores médios das variáveis -----

jaqueiras <- var1 %>%
  dplyr::filter(Campanha == "2ª") %>%
  dplyr::summarise(jaqueira = `Densidade de Jaqueiras` %>% sum(),
                   .by = c(`Unidade Amostral`, Campanha)) %>%
  tidyr::drop_na() %>%
  dplyr::slice_max(jaqueira, by = `Unidade Amostral`)

jaqueiras

dossel <- var1 %>%
  dplyr::summarise(dossel = `Índice de abertura de dossel` %>% mean(),
                   .by = c(`Unidade Amostral`, Campanha)) %>%
  tidyr::drop_na() %>%
  dplyr::slice_max(dossel, by = `Unidade Amostral`)

dossel

altura <- var2 %>%
  dplyr::summarise(altura = Altura %>% mean(),
                   .by = c(`Unidade Amostral`, Campanha)) %>%
  tidyr::drop_na() %>%
  dplyr::slice_max(altura, by = `Unidade Amostral`)

altura

inclinação <- tibble::tibble(inclinacao = inc_valores$slope,
                             `Unidade Amostral` = var2$`Unidade Amostral` %>%
                               unique() %>%
                               rep(each = 6)) %>%
  dplyr::summarise(inclinacao = inclinacao %>% mean(),
                   .by = `Unidade Amostral`) %>%
  dplyr::filter(`Unidade Amostral` != "T1P1")

inclinação

### Criando o dataframe ----

df_alfa <- tibble::tibble(`Unidade Amostral` = especies_alfa$`Unidade Amostral`,
                          Abundância = especies_alfa$Abundância,
                          `Densidade de jaqueiras` = jaqueiras$jaqueira,
                          `Abertura de Dossel` = dossel$dossel,
                          `Altura da Serrapilheira` = altura$altura,
                          `Inclinação do Terreno` = inclinação$inclinacao)

df_alfa

# Modelos lineares ----

## Multicolinearidade ----

df_alfa %>%
  dplyr::select(2:4) %>%
  cor(method = "spearman")

## Criando o modelo ----

modelo <- glm(Abundância ~ `Densidade de jaqueiras` +
                `Abertura de Dossel` +
                `Altura da Serrapilheira` +
                `Inclinação do Terreno`,
                 data = df_alfa,
                 family = poisson(link = "log"))

## Pressupostos do modelo ----

modelo %>%
  DHARMa::simulateResiduals(plot = TRUE)

## Avaliando o modelo ----

modelo %>%
  summary()

## R² ----

modelo %>% rsq::rsq()

## Gráfico ----

df_alfa2 <- df_alfa %>%
  tidyr::pivot_longer(cols = `Densidade de jaqueiras`:`Inclinação do Terreno`,
                      names_to = "Preditor",
                      values_to = "Valor Preditor")

df_alfa2

df_alfa2 %>%
  ggplot(aes(`Valor Preditor`, Abundância, fill = Preditor, color = Preditor,
             label = `Unidade Amostral`)) +
  geom_point(shape = 21, color = "black", size = 5) +
  geom_smooth(data= . %>%
                dplyr::filter(Preditor != "Inclinação do Terreno"),
              method = "lm",
              se = FALSE) +
  facet_wrap(~Preditor, scales = "free_x", ncol = 2) +
  scale_fill_manual(values = c("royalblue", "orange", "limegreen", "purple")) +
  scale_color_manual(values = c("blue", "darkorange4", "darkgreen")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 12),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "none")

ggsave(filename = "grafico_layla.png", height = 10, width = 12)
