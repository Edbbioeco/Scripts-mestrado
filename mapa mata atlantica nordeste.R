# Pacotes ----

library(tidyverse)

library(geobr)

library(cowplot)

library(sf)

# Dados ----

## Importando os dados ----

br <- geobr::read_state(showProgress = FALSE)

ne <- geobr::read_state(showProgress = FALSE) %>%
  dplyr::filter(name_region == "Nordeste")

ma <- geobr::read_biomes(showProgress = FALSE) %>%
  dplyr::filter(name_biome == "Mata Atlântica")

ucs <- geobr::read_conservation_units(showProgress = FALSE)

## Tratando os dados ----

ucs_ma <- ucs %>%
  sf::st_make_valid() %>%
  sf::st_intersection(ma)

## Visualizando os dados ----

br %>%
  ggplot() +
  geom_sf()

ne %>%
  ggplot() +
  geom_sf()

ma %>%
  ggplot() +
  geom_sf()

ucs_ma %>%
  ggplot() +
  geom_sf()

# Mapa ----

## Criação do insert map ----

insert_map_br <- ggplot() +
  geom_sf(data = br, aes(color = "Brasil", fill = "Brasil"), linewidth = 0.5) +
  geom_sf(data = ne, aes(color = "Nordeste", fill = "Nordeste"), linewidth = 0.5) +
  geom_sf(data = ma, aes(color = "Mata Atlântica", fill = "Mata Atlântica"), alpha = 0.3, linewidth = 0.5) +
  geom_rect(aes(xmin = -48.5, xmax = -34.5, ymin = -18.5, ymax = 0),
            color = "red4",
            fill = "red",
            linewidth = 0.5,
            alpha = 0.65) +
  scale_fill_manual(values = c("Brasil" = "gray70",
                               "Nordeste" = "khaki2",
                               "Mata Atlântica" = "springgreen3"),
                    breaks = c("Brasil", "Nordeste", "Mata Atlântica")) +
  scale_color_manual(values = c("Brasil" = "black",
                                "Nordeste" = "black",
                                "Mata Atlântica" = "darkgreen"),
                     breaks = c("Brasil", "Nordeste", "Mata Atlântica")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        legend.position = "none")

insert_map_br

## Criação do mapa principal ----

ma_ne <- ggplot() +
  geom_sf(data = br, aes(color = "Brasil", fill = "Brasil"), linewidth = 1) +
  geom_sf(data = ne, aes(color = "Nordeste", fill = "Nordeste"), linewidth = 1) +
  geom_sf(data = ma, aes(color = "Mata Atlântica histórica", fill = "Mata Atlântica histórica"), linewidth = 1, alpha = 0.3) +
  geom_sf(data = ucs_ma, aes(color = "Unidades de conservação", fill = "Unidades de conservação"), linewidth = 1) +
  scale_fill_manual(values = c("Brasil" = "gray70",
                               "Nordeste" = "khaki2",
                               "Mata Atlântica histórica" = "springgreen3",
                               "Unidades de conservação" = "tomato"),
                    breaks = c("Brasil", "Nordeste", "Mata Atlântica histórica", "Unidades de conservação")) +
  scale_color_manual(values = c("Brasil" = "black",
                                "Nordeste" = "black",
                                "Mata Atlântica histórica" = "darkgreen",
                                "Unidades de conservação" = "darkred"),
                     breaks = c("Brasil", "Nordeste", "Mata Atlântica histórica", "Unidades de conservação")) +
  scale_x_continuous(breaks = seq(-48.5, -34.5, 4)) +
  labs(fill = NULL,
       color = NULL) +
  coord_sf(xlim = c(-34.5, -48.5),
           ylim = c(-18.5, 0),
           label_graticule = "NSWE") +
  theme(axis.text = element_text(color = "black", size = 15),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black", fill = "transparent", linewidth = 1.5),
        panel.grid = element_line(color = "gray30", linetype = "dashed", linewidth = 0.5),
        panel.ontop = TRUE,
        legend.position = "bottom",
        legend.text = element_text(color = "black", size = 15))

ma_ne

## Juntando os mapas ----

cowplot::ggdraw(ma_ne) +
  cowplot::draw_plot(insert_map_br, width = 0.3, height = 0.4, x = 0.63, y = 0.64)

## Salvando o mapa ----

ggsave(filename = "mata atlantica nordeste.png", height = 12, width = 10)

# Área da Mata Atlântica no Nordeste ----

## Recortando a Mata Atlântica para o nordeste ----

ne_regiao <- geobr::read_region(showProgress = FALSE) %>%
  dplyr::filter(name_region == "Nordeste")

ne_regiao %>%
  ggplot() +
  geom_sf()

ma_ne_crop <- ma %>%
  sf::st_intersection(ne_regiao)

ma_ne_crop %>%
  ggplot() +
  geom_sf()

## Calculando a Área em Km² do Nordeste ----

ne_regiao %>% sf::st_area() / 1000000

## Calculando a Área em Km² da Mata Atlântica no Nordeste ----

ma_ne_crop %>% sf::st_area() / 1000000

## % de área da Mata Atlântica no Nordeste ----

((ma_ne_crop %>% sf::st_area() / 1000000) * 100) / (ne_regiao %>% sf::st_area() / 1000000)

am <- geobr::read_biomes() %>%
  dplyr::filter(name_biomes == "Amazônia")

ca <- geobr::read_biomes() %>%
  dplyr::filter(name_biomes == "Caatinga")

ce <- geobr::read_biomes() %>%
  dplyr::filter(name_biomes == "Cerrado")
