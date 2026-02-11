# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(parzer)

library(elevatr)

library(tidyterra)

library(ggtext)

# Dados ----

## Dados de abundância de espécies ----

### Importando ----

especies <- readxl::read_xlsx("levantamento_anuros.xlsx")

## Visualizando ----

especies

especies |> dplyr::glimpse()

## Shapefile de Saltinho ----

### Importando ----

saltinho <- sf::st_read("Saltinho.shp")

### Visualizando ----

saltinho

saltinho |>
  ggplot() +
  geom_sf(data = saltinho, color = "black", linewidth = 1)

## Shapefile da borda da mata ----

### Importando ----

borda <- sf::st_read("borda_saltinho.shp")

### Visualizando ----

borda

ggplot() +
  geom_sf(data = borda, color = "red", linewidth = 1) +
  geom_sf(data = saltinho, color = "black", linewidth = 1, fill = "transparent")

## Shapefile das parcelas -----

### Importnado ----

parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizando ----

parcelas

ggplot() +
  geom_sf(data = borda, color = "red", linewidth = 1) +
  geom_sf(data = saltinho, color = "black", linewidth = 1, fill = "transparent") +
  geom_sf(data = parcelas, color = "black", linewidth = 1)

## Topografia -----

### Shapefile de base para a área ----

shp_area <- tibble::tibble(x = c("35°12'18.33\"W",
                                 "35°12'18.33\"W",
                                 "35°09'16.68\"W",
                                 "35°09'16.68\"W") |>
                             parzer::parse_lon(),
                           y = c("08°42'37.43\"S",
                                 "08°44'42.43\"S",
                                 "08°44'42.43\"S",
                                 "08°42'37.43\"S") |>
                             parzer::parse_lat()) |>
  sf::st_as_sf(coords = c("x", "y"),
               crs = 4674) |>
  dplyr::summarise(geometry = geometry |>
                     sf::st_combine()) |>
  st_cast("POLYGON")

shp_area

ggplot() +
  geom_sf(data = shp_area, color = "black", fill = "green4", linewidth = 1) +
  geom_sf(data = borda, color = "red", linewidth = 1) +
  geom_sf(data = saltinho, color = "black", linewidth = 1, fill = "transparent") +
  geom_sf(data = parcelas, color = "black", linewidth = 1)

### Importandoo ----

alt <- elevatr::get_aws_terrain(locations = shp_area,
                                z = 14,
                                prj = 4674,
                                clip = "locations") |>
  terra::mask(shp_area) |>
  terra::crop(shp_area)

### Visualizando ----

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  geom_sf(data = borda, color = "red", linewidth = 1, fill = "transparent") +
  geom_sf(data = saltinho, color = "black", linewidth = 1, fill = "transparent") +
  geom_sf(data = parcelas, color = "black", linewidth = 1) +
  tidyterra::scale_fill_terrain_c()

## Corpos hídricos ----

### Importando ----

hid <- sf::st_read("corpos_hidricos_saltinho.gpkg")

### Visualizando ----

hid

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  geom_sf(data = borda, color = "red", linewidth = 1, fill = "transparent") +
  geom_sf(data = saltinho, color = "black", linewidth = 1, fill = "transparent") +
  geom_sf(data = parcelas, color = "black", linewidth = 1) +
  geom_sf(data = hid, color = "blue", linewidth = 1) +
  tidyterra::scale_fill_terrain_c()

# Abundância das espécies ----

abundancia <- especies |>
  dplyr::filter(Ordem == "Anura" &
                  Epípeto != "natalensis" &
                  Gênero != "Frostius" &
                  Família != "Hylidae") |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(`Unidade Amostral`, Espécie, Campanha)) |>
  dplyr::summarise(Abundância = Abundância |> max(),
                   .by = c(`Unidade Amostral`, Espécie))

abundancia

# Centróides das parcelas ----

## Nome das parcelas ----

nomes_parcelas <- abundancia |>
  dplyr::pull(`Unidade Amostral`) |>
  unique()

nomes_parcelas

## Calculando os centróides ----

centroides <- parcelas[-1, ] |>
  sf::st_centroid() |>
  sf::st_coordinates() |>
  tibble::as_tibble() |>
  dplyr::rename("Longitude" = 1,
                "Latitude" = 2) |>
  dplyr::mutate(`Unidade Amostral` = nomes_parcelas)

centroides

## Unindo as informações de abundância de espécies e coordenadas ----

abundancia_coord <- abundancia |>
  dplyr::left_join(centroides)

abundancia_coord

## Mapa ----

abundancia_coord |>
  dplyr::filter(Espécie %in% c("Pristimantis ramagii",
                               "Adenomera hylaedactyla",
                               "Rhinella hoogmoedi")) |>
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
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(-35.19509, -35.15463, 0.03)) +
  scale_y_continuous(expand = c(0, 0)) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = borda, aes(color = "Native Vegetation"),
          linewidth = 1, fill = "transparent") +
  geom_sf(data = saltinho, aes(color = "REBio Saltinho"),
          linewidth = 1, fill = "transparent") +
  geom_sf(data = parcelas, aes(color = "Samples"),
          linewidth = 1) +
  scale_color_manual(values = c("REBio Saltinho" = "black",
                                "Native Vegetation" = "darkgreen",
                                "Samples" = "royalblue4")) +
  geom_point(aes(Longitude, Latitude, fill = Abundância),
                 shape = 21, size = 5, stroke = 1, color = "black", width = 0.1) +
  scale_fill_viridis_c(name = "Species abundance",
                       guide = guide_colorbar(order = 2,
                                              title.position = "top",
                                              title.hjust = 0.5,
                                              barwidth = 15,
                                              frame.colour = "black",
                                              frame.linewidth = 1,
                                              ticks.colour = "black",
                                              ticks.linewidth = 1)) +
  facet_wrap(~Espécie, ncol = 2) +
  labs(x = NULL,
       y = NULL,
       colour = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15, face = "italic"),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = c(1, 0.15),
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.justification = c("right", "bottom"),
        legend.background = element_blank())

ggsave(filename = "mapa_especies_abundancia.png", height = 10, width = 12)

## Versão com pontos com tamanhos diferentes ----

### Criando um data frame prévio ----

df_pontos_abu <- abundancia_coord |>
  dplyr::filter(Espécie %in% c("Pristimantis ramagii",
                               "Adenomera hylaedactyla",
                               "Rhinella hoogmoedi")) |>
  dplyr::mutate(Espécie = dplyr::case_when(Espécie == "Adenomera hylaedactyla" ~ "Adenomera aff. hylaedactyla",
                                           .default = Espécie),
                Espécie = paste0("<i>", Espécie, "</i>"),
                Espécie = dplyr::if_else(Espécie |> stringr::str_detect("aff.|gr.|cf.|aff|gr|cf"),
                                         Espécie |>
                                           stringr::str_replace_all(c(" aff " = "</i> aff. <i>",
                                                                      " aff. " = "</i> aff. <i>",
                                                                      " gr " = "</i> gr. <i>",
                                                                      " gr. " = "</i> gr. <i>",
                                                                      " cf " = "</i> cf. <i>",
                                                                      " cf. " = "</i> cf. <i>")),
                                         Espécie),
                Espécie = Espécie |>
                  forcats::fct_relevel(c("<i>Pristimantis ramagii</i>",
                                         "<i>Adenomera</i> aff. <i>hylaedactyla</i>",
                                         "<i>Rhinella hoogmoedi</i>")))

df_pontos_abu

### Gráfico ----

df_pontos_abu |>
  ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  tidyterra::scale_fill_whitebox_c(palette = "arid",
                                   direction = -1,
                                   name = "Altitude",
                                   guide = guide_colorbar(order = 1,
                                                          title.position = "top",
                                                          title.hjust = 0.5,
                                                          barwidth = 15,
                                                          frame.colour = "black",
                                                          frame.linewidth = 1,
                                                          ticks.colour = "black",
                                                          ticks.linewidth = 1)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(-35.19509, -35.15463, 0.03)) +
  scale_y_continuous(expand = c(0, 0)) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = hid, aes(color = "Hidric Bodies"),
          fill = "blue",
          linewidth = 1) +
  geom_sf(data = borda, aes(color = "Native Vegetation"),
          linewidth = 1, fill = "transparent") +
  geom_sf(data = parcelas, aes(color = "Samples"),
          linewidth = 1) +
  scale_color_manual(values = c("Native Vegetation" = "darkgreen",
                                "Samples" = "royalblue4",
                                "Hidric Bodies" = "blue")) +
  geom_point(aes(Longitude, Latitude, size = Abundância),
             shape = 21, stroke = 1, color = "black", fill = "green4", width = 0.1) +
  scale_size_continuous(breaks = seq(1, 35, 5),
                        guide = guide_legend(order = 2,
                                             title = "Abundance",
                                             title.position = "top",
                                             title.hjust = 0.5),
                        range = c(1, 10)) +
  facet_wrap(~Espécie, ncol = 2) +
  labs(x = NULL,
       y = NULL,
       colour = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        strip.text = ggtext::element_markdown(color = "black", size = 15),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = c(1, 0.08),
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.justification = c("right", "bottom"),
        legend.background = element_blank()) +
  ggview::canvas(height = 10, width = 15)

ggsave(filename = "mapa_especies_abundancia_2.png", height = 10, width = 15)

