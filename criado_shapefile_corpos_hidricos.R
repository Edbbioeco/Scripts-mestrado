# Pacotes ----

library(sf)

library(tidyverse)

# Dados ----

## Saltinho ----

### Importando ----

saltinho <- sf::st_read("Saltinho.shp")

### Visualizando ----

saltinho

ggplot() +
  geom_sf(data = saltinho, color = "black", linewidth = 1)

## Açude 1 ----

### Importando ----

açude1 <- sf::st_read("açude 1.kml")

### Visualizando ----

açude1

ggplot() +
  geom_sf(data = saltinho, color = "black", linewidth = 1) +
  geom_sf(data = açude1, color = "blue", fill = "blue",
          alpha = 0.5, linewidth = 1)

## Açude 2 ----

### Importando ----

açude2 <- sf::st_read("açude 2.kml")

### Visualizando ----

açude2

ggplot() +
  geom_sf(data = saltinho, color = "black", linewidth = 1) +
  geom_sf(data = açude1, color = "blue", fill = "blue",
          alpha = 0.5, linewidth = 1) +
  geom_sf(data = açude2, color = "blue", fill = "blue",
          alpha = 0.5, linewidth = 1)

## Curso d'água 1 ----

### Importando ----

curso1 <- sf::st_read("curso 1.kml")

### Visualizando ----

curso1

ggplot() +
  geom_sf(data = saltinho, color = "black", linewidth = 1) +
  geom_sf(data = açude1, color = "blue", fill = "blue",
          alpha = 0.5, linewidth = 1) +
  geom_sf(data = açude2, color = "blue", fill = "blue",
          alpha = 0.5, linewidth = 1) +
  geom_sf(data = curso1, color = "blue", linewidth = 1)

## Curso d'água 2 ----

### Importando ----

curso2 <- sf::st_read("curso 2.kml")

### Visualizando ----

curso2

ggplot() +
  geom_sf(data = saltinho, color = "black", linewidth = 1) +
  geom_sf(data = açude1, color = "blue", fill = "blue",
          alpha = 0.5, linewidth = 1) +
  geom_sf(data = açude2, color = "blue", fill = "blue",
          alpha = 0.5, linewidth = 1) +
  geom_sf(data = curso1, color = "blue", linewidth = 1) +
  geom_sf(data = curso2, color = "blue", linewidth = 1)

## Curso d'água 3 ----

### Importando ----

curso3 <- sf::st_read("curso 3.kml")

### Visualizando ----

curso3

ggplot() +
  geom_sf(data = saltinho, color = "black", linewidth = 1) +
  geom_sf(data = açude1, color = "blue", fill = "blue",
          alpha = 0.5, linewidth = 1) +
  geom_sf(data = açude2, color = "blue", fill = "blue",
          alpha = 0.5, linewidth = 1) +
  geom_sf(data = curso1, color = "blue", linewidth = 1) +
  geom_sf(data = curso2, color = "blue", linewidth = 1) +
  geom_sf(data = curso3, color = "blue", linewidth = 1)

# Unindo os shapefiles ----

## Unindo ----

sf_hidrico <- dplyr::bind_rows(açude1,
                               açude2,
                               curso1,
                               curso2,
                               curso3) |>
  dplyr::mutate(Tipo = c(rep("Açude", 2),
                         rep("Curso d'água", 3)))

sf_hidrico

## Exportando ----

sf_hidrico |>
  sf::st_set_crs(4674) |>
  sf::st_write("corpos_hidricos_saltinho.gpkg")

## Testando ----

hdc <- sf::st_read("corpos_hidricos_saltinho.gpkg")

ggplot() +
  geom_sf(data = saltinho, color = "black", linewidth = 1) +
  geom_sf(data = hdc, color = "blue", fill = "blue",
          alpha = 0.5, linewidth = 1)
