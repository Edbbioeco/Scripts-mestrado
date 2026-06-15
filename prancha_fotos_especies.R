# Pacotes ---

library(tidyverse)

library(terra)

library(tidyterra)

library(patchwork)

# Fotos ----

## Lista de fotos ----

foto <- list.files(pattern = "^foto_")

foto

## Importando ----

fotos_unidas <- purrr::map(foto,
           purrr::in_parallel(~terra::rast(.x) |>
                                terra::flip(direction = "vertical")),
           .progress = TRUE) |>
  setNames(foto |>
             stringr::str_replace_all("_", " ") |>
             stringr::str_remove(".jpg|.jpeg|.JPG") |>
             stringr::str_extract("\\w+$"))

## Visualizando ----

fotos_unidas

purrr::map(fotos_unidas,
           purrr::in_parallel(
            ~ggplot() +
              tidyterra::geom_spatraster_rgb(data = .x) +
              coord_sf(expand = FALSE) +
              coord_equal() +
              theme_void()),
           .progress = TRUE)

# Prancha ----

## Nome das fotos na lista ----

nome_especies <- c("ramagii",
                   "hylaedactyla",
                   "hoogmoedi")

nome_especies

## Posição das letras nas fotos ----

id_letras <- purrr::pmap(list(fotos_unidas[c(2, 1, 3)],
                              nome_especies,
                              LETTERS[1:3]),

                         \(fotos_unidas, nome_especies, letras){

                             extenc <- fotos_unidas |> terra::ext()

                             x_pos <- extenc[2]/8

                             y_pos <- (extenc[2]/8)*7

                             tibble::tibble(x = x_pos,
                                            y = y_pos,
                                            letra = letras,
                                            id = nome_especies)

                             },
                         .progress = TRUE) |>
  dplyr::bind_rows()

id_letras

## Criando os ggplots ----

lista_ggplots <- purrr::map2(fotos_unidas[c(2, 1, 3)],
                             nome_especies,
                             purrr::in_parallel(

                               ~ggplot() +
                                 tidyterra::geom_spatraster_rgb(data = .x) +
                                 coord_sf(expand = FALSE) +
                                 coord_equal() +
                                 geom_text(data = id_letras |>
                                             dplyr::filter(id == .y),
                                           aes(x, y, label = letra),
                                           size = 10,
                                           color = "white",
                                           fontface = "bold") +
                                 theme_void()

                               ),
                             .progress = TRUE) |>
  setNames(paste0("ggplot_", nome_especies))

lista_ggplots

## Criando a prancha ----

### Horizontal ----

prancha_horizontal <- patchwork::wrap_plots(lista_ggplots) +
  theme(plot.tag = element_text(face = "bold", size = 20, hjust = -0.5),
        plot.tag.position = c(0, 1))

prancha_horizontal

prancha_horizontal + ggview::canvas(height = 4.5, width = 12)

ggsave(filename = "prancha_species_horizontal.png",
       height = 4.5,
       width = 12)

### Vertical ----

prancha_vertical <- patchwork::wrap_plots(lista_ggplots,
                                          ncol = 1) +
  theme(plot.tag = element_text(face = "bold", size = 20, hjust = -0.5),
        plot.tag.position = c(0, 1))

prancha_vertical

prancha_vertical + ggview::canvas(height = 12, width = 4.5)

ggsave(filename = "prancha_species_vertical.png",
       height = 12,
       width = 4.5)
