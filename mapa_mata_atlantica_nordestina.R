# Pacotes ----

library(sf)

library(tidyverse)

# Dados ----

## Shapefile dos Estados do Brasil ----

### Importar ----

br <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/aula_geoespacial/br.shp")

### Visualizar ----

br

ggplot() +
  geom_sf(data = br, color = "black")

## Shapefile dos estados do Nordeste ----

### Filtrar ----

ne <- br |>
  dplyr::filter(cod_rgn == 2)

### Visualizar ----

ne

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = ne, color = "black", fill = "goldenrod")

## Mata Atlântica ----

### Importar ----

ma <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/aula_geoespacial/biomas.shp") |>
  dplyr::filter(name_biome == "Mata Atlântica")

### Visualizar ----

ma

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = ne, color = "black", fill = "goldenrod") +
  geom_sf(data = ma, color = "darkgreen", fill = "transparent")

## Sahepfile das unidades de conservaão da Mata Altântica Nordestina ----

### Importar e recortar ----

uc <- sf::st_read("C:/Users/LENOVO/OneDrive/Documentos/aula_geoespacial/unidade_conservacao.shp") |>
  sf::st_intersection(ma |>
                        sf::st_intersection(ne |>
                                              sf::st_union()))

### Visulizar ----

uc

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = ne, color = "black", fill = "goldenrod") +
  geom_sf(data = ma, color = "darkgreen", fill = "transparent") +
  geom_sf(data = uc, color = "darkred", fill = "darkred", alpha = 0.3)

# Mapa ----

ggplot() +
  geom_sf(data = br,
          aes(color = "Brasil", fill = "Brasil"),
          linewidth = 1) +
  geom_sf(data = ne,
          aes(color = "Nordeste", fill = "Nordeste"),
          linewidth = 1) +
  geom_sf(data = ma,
          aes(color = "Mata Atlântica", fill = "Mata Atlântica"),
          alpha = 0.5, linewidth = 1) +
  geom_sf(data = uc,
          aes(color = "Unidades de Conservação", fill = "Unidades de Conservação"),
          alpha = 0.5) +
  coord_sf(xlim = c(-48.75515, -32.37777),
           ylim = c(-18.34849, -1.04971),
           expand = FALSE,
           label_graticule = "NSWE") +
  scale_color_manual(values = c("Brasil" = "black",
                                "Nordeste" = "black",
                                "Mata Atlântica" = "darkgreen",
                                "Unidades de Conservação" = "darkred"),
                     breaks = c("Brasil",
                                "Nordeste",
                                "Mata Atlântica",
                                "Unidades de Conservação")) +
  scale_fill_manual(values = c("Brasil" = "gray",
                               "Nordeste" = "goldenrod",
                               "Mata Atlântica" = "darkgreen",
                               "Unidades de Conservação" = "darkred"),
                    breaks = c("Brasil",
                               "Nordeste",
                               "Mata Atlântica",
                               "Unidades de Conservação")) +
  labs(fill = NULL,
       color = NULL) +
  theme_minimal() +
  theme(axis.text = element_text(color = "black", size = 25),
        legend.text = element_text(color = "black", size = 25),
        legend.title = element_text(color = "black", size = 25),
        legend.position = "bottom") +
  ggview::canvas(height = 12, width = 12)

ggsave(filename = "mapa_mata_atlantica_nordestina.png",
       height = 12, width = 12)
