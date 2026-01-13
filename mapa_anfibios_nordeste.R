# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(geobr)

library(sf)

# Dados ----

## Corrdenadas ----

### Importando ----

dados <- readxl::read_xlsx("inventario_anfibios_caatinga.xlsx",
                           sheet = 2)

### Visualizando ----

dados %>% View()

dados %>% dplyr::glimpse()

### Tratando ----

dados_trat <- dados %>%
  dplyr::mutate(Longitude = Longitude %>% parzer::parse_lon(),
                Latitude = Latitude %>% parzer::parse_lat())

dados_trat

## Shapefiles ----

### Importando ----

ne <- geobr::read_state() %>%
  dplyr::filter(name_region == "Nordeste")

NE <- geobr::read_region() %>%
  dplyr::filter(name_region == "Nordeste")

biomas <- geobr::read_biomes()

### Visualizando ----

ne %>%
  ggplot() +
  geom_sf()

NE %>%
  ggplot() +
  geom_sf()

biomas %>%
  ggplot() +
  geom_sf()

### Tratando ----

biomas_trat <- biomas %>%
  tidyr::drop_na() %>%
  sf::st_intersection(NE)

biomas_trat

biomas_trat %>%
  ggplot() +
  geom_sf()

# Visualizando os pontos ----

ggplot() +
  geom_sf(data = biomas_trat, aes(fill = name_biome, color = name_biome)) +
  geom_sf(data = ne, color = "black", fill = "transparent") +
  geom_point(data = dados_trat, aes(Longitude, Latitude)) +
  labs(fill = "Bioma",
       color = "Bioma",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("darkgreen", "gold", "orange", "green4")) +
  scale_color_manual(values = c("darkgreen", "gold", "orange", "green4")) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom",
        panel.grid = element_line(linetype = "dashed", color = "black"),
        panel.background = element_rect(fill = "transparent", color = "black"))
