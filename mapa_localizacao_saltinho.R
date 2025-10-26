# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

library(geodata)

library(terra)

library(tidyterra)

library(ggspatial)

library(patchwork)

# Dados ----

## Pontos de Coleta ----

coord <- readxl::read_xlsx("coordenadas_proposta.xlsx", sheet = 2) %>%
  dplyr::mutate(Longitude = Longitude %>% parzer::parse_lon(),
                Latitude = Latitude %>% parzer::parse_lat(),
                Parcela = dplyr::case_when(Trilha == "Ripária" ~ "Ripária",
                                           .default = "De Trilhas")) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = saltinho %>% sf::st_crs())

coord

coord %>%
  ggplot() +
  geom_sf(aes(fill = Parcela), shape = 21, color = "black", size = 5)

## Shapefiles ----

### Brasil ----

br <- geobr::read_country()

br

br %>%
  ggplot() +
  geom_sf()

### Continentes ----

continentes <- geodata::world(path = here::here()) %>%
  sf::st_as_sf() %>%
  dplyr::filter(NAME_0 != "Brazil")

continentes

continentes %>%
  ggplot() +
  geom_sf()

### Pernambuco ----

pe <- geobr::read_state() %>%
  dplyr::filter(name_state == "Pernambuco")

pe

pe %>%
  ggplot() +
  geom_sf()

### Saltinho ----

saltinho <- geobr::read_conservation_units() %>%
  dplyr::filter(name_conservation_unit %>% stringr::str_detect("SALTINHO") == TRUE)

saltinho

saltinho %>%
  ggplot() +
  geom_sf()

### Trilhas ----

trilhas <- readxl::read_xlsx("coordenadas_proposta.xlsx") %>%
  dplyr::filter(Trilha != "Ripária") %>%
  dplyr::mutate(Longitude = Longitude %>% parzer::parse_lon(),
                Latitude = Latitude %>% parzer::parse_lat()) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = saltinho %>% sf::st_crs()) %>%
  dplyr::summarise(geometry = sf::st_combine(geometry), .by = Trilha) %>%
  sf::st_cast("LINESTRING")

trilhas

trilhas %>%
  ggplot() +
  geom_sf()

## Raster ----

saltinho_rast <- terra::rast("saltinho.tif")

saltinho_rast

ggplot() +
  tidyterra::geom_spatraster_rgb(data = saltinho_rast)

# Mapa ----

## Mapa brasil ----

mapa_br <- ggplot() +
  geom_sf(data = continentes, color = "black", aes(fill = "América do Sul"), linewidth = 0.5) +
  geom_sf(data = saltinho, color = "red", aes(fill = "Saltinho"), linewidth = 1) +
  geom_sf(data = br, color = "black", aes(fill = "Brasil"), linewidth = 0.5) +
  geom_sf(data = pe, color = "black", aes(fill = "Pernambuco"), linewidth = 0.5) +
  geom_sf(data = trilhas, aes(color = "Trilhas"), linewidth = 1, fill = "transparent") +
  scale_fill_manual(values = c("América do Sul" = "gray40",
                               "Brasil" = "white",
                               "Pernambuco" = "#FFF085",
                               "Saltinho" = "transparent"),
                    guide = guide_legend(order = 1)) +
  scale_color_manual(values = "gold4",
                     guide = guide_legend(order = 2)) +
  labs(fill = NULL,
       color = NULL) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = coord, aes(fill = Parcela),
          shape = 21, color = "black", size = 5,
          alpha = 0) +
  scale_fill_manual(values = c("cyan4",
                               "gold"),
                    guide = guide_legend(order = 3)) +
  coord_sf(xlim = c(-72.5, -34.994), ylim = c(-32.5, 4.5), label_graticule = "WNS") +
  labs(fill = NULL,
       color = NULL) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  scale_x_continuous(breaks = seq(-70, -40, 10)) +
  scale_y_continuous(breaks = seq(-30, 5, 10)) +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.line = element_line(color = "black"),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.background = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.position = "bottom",
        plot.margin = margin(t = 0,
                             r = 0,
                             b = 0,
                             l = 0))

mapa_br

## Mapa Pernambuco ----

mapa_pe <- ggplot() +
  geom_sf(data = continentes, color = "black", aes(fill = "América do Sul"), linewidth = 0.5) +
  geom_sf(data = br, color = "black", aes( fill = "Brasil"), linewidth = 0.5) +
  geom_sf(data = pe, color = "black", aes(fill = "Pernambuco"), linewidth = 0.5) +
  geom_sf(data = saltinho, color = "red", aes(fill = "Saltinho"), linewidth = 1) +
  geom_sf(data = trilhas, aes(color = "Trilhas"), linewidth = 1, fill = "transparent") +
  scale_fill_manual(values = c("América do Sul" = "gray40",
                               "Brasil" = "white",
                               "Pernambuco" = "#FFF085",
                               "Saltinho" = "transparent"),
                    guide = guide_legend(order = 1)) +
  scale_color_manual(values = "gold4",
                     guide = guide_legend(order = 2)) +
  labs(fill = NULL,
       color = NULL) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = coord, aes(fill = Parcela),
          shape = 21, color = "black", size = 5,
          alpha = 0) +
  scale_fill_manual(values = c("cyan4",
                               "gold"),
                    guide = guide_legend(order = 3)) +
  coord_sf(ylim = c(-9, -8), xlim = c(-37, -35), label_graticule = "EN") +
  labs(fill = NULL,
       color = NULL) +
  scale_y_continuous(breaks = seq(-9, -8, 0.4)) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.line = element_line(color = "black"),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.background = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.position = "none")


mapa_pe

## Mapa Saltinho ----

saltinho_rast

mapa_saltinho <- ggplot() +
  geom_sf(data = continentes, color = "black", aes(fill = "América do Sul"), linewidth = 0.5) +
  geom_sf(data = br, color = "black", aes( fill = "Brasil"), linewidth = 0.5) +
  geom_sf(data = pe, color = "black", aes(fill = "Pernambuco"), linewidth = 0.5) +
  tidyterra::geom_spatraster_rgb(data = saltinho_rast) +
  geom_sf(data = saltinho, color = "red", aes(fill = "Saltinho"), linewidth = 1) +
  geom_sf(data = trilhas, aes(color = "Trilhas"), linewidth = 1, fill = "transparent") +

  scale_fill_manual(values = c("América do Sul" = "gray40",
                               "Brasil" = "white",
                               "Pernambuco" = "#FFF085",
                               "Saltinho" = "transparent"),
                    guide = guide_legend(order = 1)) +
  scale_color_manual(values = "gold4",
                     guide = guide_legend(order = 2)) +
  labs(fill = NULL) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = coord, aes(fill = Parcela), shape = 21, color = "black", size = 5) +
  scale_fill_manual(values = c("cyan4",
                               "gold"),
                    guide = guide_legend(order = 3)) +
  scale_x_continuous(expand = c(0,0), breaks = seq(-35.20, -35.16, 0.02)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-8.740, -8.715, 0.01)) +
  coord_sf(xlim = c(-35.20319, -35.15696), ylim = c(-8.744113, -8.710025), label_graticule = "ES") +
  labs(fill = NULL,
       color = NULL) +
  ggspatial::annotation_scale(location = "bl",
                              height = unit(0.4, "cm"),
                              bar_cols = c("black", "yellow"),
                              text_face = "bold",
                              text_cex = 1) +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.line = element_line(color = "black"),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.background = element_blank(),
        legend.text = element_text(color = "black", size = 15))

mapa_saltinho

## Unindo ----

mapa_unido <- (mapa_pe / mapa_saltinho) +
  plot_layout(guides = "collect") &
  theme(plot.margin = unit(c(0, 0, 0, 0), "mm"))

mapa_unido

(mapa_br + plot_layout(guides = "collect") & theme(legend.position = "bottom")) +
  (mapa_unido & theme(legend.position = "none")) &
  theme(plot.margin = unit(c(1, 0, 0, 1), "mm"))

ggsave("mapa_saltinho_localizacao.png", height = 12, width = 14)
