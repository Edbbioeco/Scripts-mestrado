# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(readxl)

library(parzer)

library(elevatr)

# Dados ----

## Shapefile saltinho ----

### Importando ----

saltinho <- sf::st_read("saltinho.shp")

### Visualizando ----

saltinho %>%
  ggplot() +
  geom_sf(color = "red", fill = "transparent", linewidth = 1)

## Imagem de satélite ----

### Importando ----

img <- terra::rast("Saltinho.tif")

### Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img) +
  geom_sf(data = saltinho, color = "red", fill = "transparent", linewidth = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

## Raster de elevação ----

### Importando ----

elev <- elevatr::get_elev_raster(locations = saltinho, z = 14) %>%
  terra::rast() %>%
  terra::crop(saltinho) %>%
  terra::mask(saltinho)

### Criando o shapefile de curva de terreno ----

elev_niv <- elev %>%
  terra::as.contour(levels = seq(0, 200, 5)) %>%
  sf::st_as_sf()

### Visualizando ----

elev

elev_niv

ggplot() +
  tidyterra::geom_spatraster(data = elev) +
  geom_sf(data = elev_niv) +
  geom_sf(data = saltinho, color = "red", fill = "transparent", linewidth = 1) +
  coord_sf(xlim = c(-35.19784, -35.16593), ylim = c(-8.739185, -8.712074),
           label_graticule = "NWSE") +
  scale_fill_viridis_c(na.value = "transparent")


## Coordenadas ----

### Importando ----

coords <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/parcelas_saltinho.xlsx",
                            sheet = 2)
coords |> as.data.frame()
### tratando ----

coords_trat <- coords %>%
  dplyr::filter(!Latitude %>% is.na) %>%
  dplyr::mutate(`Trilha-Parcela` = paste0(paste0("Trilha ", Trilha),
                                          "-",
                                          paste0("Parcela ", Parcela)),
                Latitude = Latitude %>% parzer::parse_lat(),
                Longitude = Longitude %>% parzer::parse_lon()) %>%
  tidyr::unite(col = "Trilha-Parcela",
               sep = "-",
               Trilha:Parcela) %>%
  dplyr::group_by(`Trilha-Parcela`) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = saltinho %>% sf::st_crs()) %>%
  dplyr::group_by(`Trilha-Parcela`) %>%
  dplyr::summarise(do_union = FALSE) %>%
  sf::st_cast("LINESTRING")

### Visualizando ----

coords_trat

### Exportando ----

coords_trat %>%
  sf::st_write("saltinho_ppbio_parcelas.shp")

# Mapas ----

## Imagem de satélite ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img) +
  geom_sf(data = saltinho, aes(color = "REBio Saltinho"), fill = "transparent", linewidth = 1) +
  labs(colour = "") +
  geom_sf(data = coords_trat,
             aes(color = `Trilha-Parcela`)) +
  coord_sf(xlim = c(-35.19784, -35.16593), ylim = c(-8.739185, -8.712074),
           label_graticule = "NWSE") +
  labs(x = NULL,
       y = NULL) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5),
         color = guide_legend(title.position = "top",
                              title.hjust = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 12),
        legend.position = "bottom")

ggsave(filename = "saltinho_parcelas_mapa.png", height = 12, width = 14)

## Nível de terreno ----

### Satelite ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img, alpha = 0.825) +
  geom_sf(data = elev_niv) +
  geom_sf(data = saltinho, aes(color = "REBio Saltinho"), fill = "transparent", linewidth = 1) +
  scale_color_manual(values = c("REBio Saltinho" = "red")) +
  labs(colour = "") +
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale_fill() +
  geom_path(data = coords_trat, aes(Longitude, Latitude)) +
  geom_point(data = coords_trat, aes(Longitude, Latitude), shape = 21, color = "black") +
  coord_sf(xlim = c(-35.19784, -35.16593), ylim = c(-8.739185, -8.712074),
           label_graticule = "NWSE") +
  labs(x = NULL,
       y = NULL) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5),
         color = guide_legend(title.position = "top",
                              title.hjust = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom")

ggsave(filename = "saltinho_parcelas_mapa_nivel.png", height = 12, width = 14)

### Elevação ----

ggplot() +
  tidyterra::geom_spatraster(data = elev) +
  scale_fill_viridis_c(na.value = "transparent",
                       breaks = seq(30, 160, 10),
                       limits = c(30, 160)) +
  geom_sf(data = elev_niv) +
  geom_sf(data = saltinho, aes(color = "REBio Saltinho"), fill = "transparent", linewidth = 1) +
  scale_color_manual(values = c("REBio Saltinho" = "red")) +
  labs(colour = "",
       fill = "Elevação") +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 30,
                                barheight = 2,
                                frame.colour = "black",
                                ticks.colour = "black",
                                ticks.linewidth = 0.5)) +
  coord_sf(xlim = c(-35.19784, -35.16593), ylim = c(-8.739185, -8.712074),
           label_graticule = "NWSE") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom")

ggsave(filename = "saltinho_mapa_nivel.png", height = 12, width = 14)
