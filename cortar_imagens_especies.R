# Pacotes ----

library(tidyverse)

library(terra)

library(tidyterra)

library(coiR)

# Imagens ----

## Importar ----

imagens <- purrr::map(list.files(path = "C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/imagens_especies",
                      full.names = TRUE),
                      terra::rast) |>
  setNames(list.files(path = "C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/imagens_especies") |>
             stringr::str_remove_all(".jppeg|.JPG"))

imagens

### Visualizar ----

purrr::map(imagens,
           purrr::in_parallel(

             ~ggplot() +
               tidyterra::geom_spatraster_rgb(data = .x) +
               coord_sf(expand = FALSE)

           ),
           .progress = TRUE)
