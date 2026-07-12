# Pacotes ----

library(tidyverse)

library(terra)

library(tidyterra)

library(coiR)

library(magick)

# Imagens ----

## Importar ----

imagens <- purrr::map(list.files(path = "C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/imagens_especies",
                      full.names = TRUE),
                      ~terra::rast(.x) |>
                        terra::flip()) |>
  setNames(list.files(path = "C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/imagens_especies") |>
             stringr::str_remove_all(".jpeg|.JPG"))

imagens

### Visualizar ----

purrr::map(imagens,
           purrr::in_parallel(

             ~ggplot() +
               tidyterra::geom_spatraster_rgb(data = .x) +
               coord_sf(expand = FALSE) +
               coord_equal()

           ),
           .progress = TRUE)

# Recortar as imagens para circular ----

## Recortar ----

imagens_cortadas <- purrr::map(imagens,
                               purrr::in_parallel(

                                 ~coiR::coir_crop(data = .x)

                               ),
                               .progress = TRUE)

imagens_cortadas

### Exportar ----

purrr::iwalk(imagens_cortadas,
             ~{

               alfa <- terra::app(.x[[1]],
                                  \(x) ifelse(is.na(x), 0, 255))

               rgba <- c(.x, alfa)

               terra::RGB(rgba, alpha = 4) <- 1:4

               rgba |> terra::writeRaster(
                 paste0("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/imagens_especies/",
                        .y,
                        "_cortada.png"),
                 overwrite = TRUE,
                 filetype = "PNG")

               },
             .progress = TRUE)
