# Pacotes

library(tidyverse)

library(geobr)

library(sf)

# dados

## Importando

ma <- geobr::read_biomes(showProgress = FALSE) %>%
  dplyr::filter(name_biome == "Mata Atlântica")

am <- geobr::read_biomes(showProgress = FALSE) %>%
  dplyr::filter(name_biome == "Amazônia")

ca <- geobr::read_biomes(showProgress = FALSE) %>%
  dplyr::filter(name_biome == "Caatinga")

ce <- geobr::read_biomes(showProgress = FALSE) %>%
  dplyr::filter(name_biome == "Cerrado")

ne <- geobr::read_region(showProgress = FALSE) %>%
  dplyr::filter(name_region == "Nordeste")

## Visualizando

ma %>%
  ggplot() +
  geom_sf()

am %>%
  ggplot() +
  geom_sf()

ca %>%
  ggplot() +
  geom_sf()

ce %>%
  ggplot() +
  geom_sf()

ne %>%
  ggplot() +
  geom_sf()

## Recortando

### Recorte

ma_crop <- ma %>%
  sf::st_intersection(ne)

am_crop <- am %>%
  sf::st_intersection(ne)

ca_crop <- ca %>%
  sf::st_intersection(ne)

ce_crop <- ce %>%
  sf::st_intersection(ne)

### Visualizando

ma_crop %>%
  ggplot() +
  geom_sf()

am_crop %>%
  ggplot() +
  geom_sf()

ca_crop %>%
  ggplot() +
  geom_sf()

ce_crop %>%
  ggplot() +
  geom_sf()

# Área em Km²

## Nordeste

ne_area <- ne %>%
  sf::st_area() / 1000000

ne_area

## Biomas

ma_area <- ma_crop %>%
  sf::st_area() / 1000000

ma_area

am_area <- am_crop %>%
  sf::st_area() / 1000000

am_area

ca_area <- ca_crop %>%
  sf::st_area() / 1000000

ca_area

ce_area <- ce_crop %>%
  sf::st_area() / 1000000

ce_area

# Visualizando

## Biomas unidos no nordeste

ggplot() +
  geom_sf(data = ma_crop, fill = "green3", color = "green3") +
  geom_sf(data = ca_crop, fill = "gold", color = "gold") +
  geom_sf(data = ce_crop, fill = "orange", color = "orange") +
  geom_sf(data = am_crop, fill = "darkgreen", color = "darkgreen") +
  geom_sf(data = ne, fill = "transparent", color = "black", linewidth = 1) +
  theme_bw()

## Distribuião de tamanho de áreas

### Criando um novo objeto

biomas_areas <- dplyr::bind_rows(ma_crop, am_crop, ca_crop, ce_crop) %>%
  dplyr::mutate(`Área (km²)` = c(ma_area %>% as.numeric(),
                                 am_area %>% as.numeric(),
                                 ca_area %>% as.numeric(),
                                 ce_area %>% as.numeric()))

biomas_areas

### Criando o mapa

ggplot() +
  geom_sf(data = biomas_areas, aes(fill = `Área (km²)`, color = `Área (km²)`)) +
  scale_fill_viridis_c(option = "turbo") +
  scale_color_viridis_c(option = "turbo") +
  geom_sf(data = ne, fill = "transparent", color = "black", linewidth = 1) +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 15,
                                barheight = 1,
                                frame.colour = "black",
                                ticks.colour = "black")) +
  theme_bw() +
  theme(legend.position = "bottom")

### Criando um gráfico

biomas_areas %>%
  ggplot(aes(name_biome, `Área (km²)`, fill = name_biome)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c("darkgreen", "gold", "orange", "green3")) +
  labs(x = "Bioma") +
  theme_bw()

# Novo mapa dos biomas

ggplot() +
  geom_sf(data = biomas_areas, aes(fill = name_biome, color = name_biome)) +
  scale_fill_manual(values = c("darkgreen", "gold", "orange", "green3")) +
  scale_color_manual(values = c("darkgreen", "gold", "orange", "green3")) +
  geom_sf(data = ne, fill = "transparent", color = "black", linewidth = 1) +
  guides(fill = guide_legend(title = "Bioma",
                             title.position = "top",
                             title.hjust = 0.5),
         color = guide_legend(title = "Bioma",
                             title.position = "top",
                             title.hjust = 0.5)) +
  theme_bw() +
  theme(legend.position = "bottom")
