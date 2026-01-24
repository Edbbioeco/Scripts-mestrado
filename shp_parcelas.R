# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(sf)

# Dadpos ----

## Coordenadas ----

### Importando ----

coords <- readxl::read_xlsx("G:/Meu Drive/UFPE/projeto mestrado/parcelas_saltinho.xlsx",
                            sheet = 2)

### Visualizzando ----

coords %>% dplyr::glimpse()

coords

### Tratando ----

coords_trat <- coords %>%
  dplyr::filter(!Latitude %>% is.na) %>%
  dplyr::mutate(`Trilha-Parcela` = paste0(paste0("Trilha ", Trilha),
                                          "-",
                                          paste0("Parcela ", Parcela)),
                Latitude = Latitude %>% parzer::parse_lat(),
                Longitude = Longitude %>% parzer::parse_lon())

coords_trat %>% dplyr::glimpse()

coords_trat

## Shapefile Saltinho ----

### Importando ----

saltinho <- sf:::st_read("saltinho.shp")

### Visualizzando ----

saltinho

saltinho %>%
  ggplot() +
  geom_sf()

# Shapefile ----

## Importando um crs ----

crs <- saltinho %>%
  sf::st_crs()

crs

## Criando o shapefile ----

coord_sf <- coords_trat %>%
  dplyr::group_by(`Trilha-Parcela`) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = crs) %>%
  dplyr::summarise(do_union = FALSE) %>%
  sf::st_cast("LINESTRING")

coord_sf

ggplot() +
  geom_sf(data = saltinho, color = "red", linewidth = 1) +
  geom_sf(data = coord_sf)

## Exportando ---

### KML ----

coord_sf %>%
  sf::st_write("coordenadas_saltinho.kml")

### SHP ----

coord_sf %>%
  sf::st_write("coordenadas_saltinho.shp")

coords %>%
  dplyr::filter(Piquete %in% c(0, 10)) %>%
  dplyr::select(Trilha:Piquete, Latitude:Longitude) %>%
  tidyr::drop_na() %>%
  dplyr::filter(!(Trilha == "Ripária" & Piquete == 10) &
                  !(Trilha == 2 & Piquete == 250)) %>%
  dplyr::mutate(Latitude = Latitude %>% parzer::parse_lat(),
                Longitude = Longitude %>% parzer::parse_lon()) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = geobr::read_country() %>%
                 sf::st_crs()) %>%
  sf::st_write("coordenadas_parcelas_saltinho.shp")
