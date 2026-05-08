# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(terra)

library(tidyterra)

library(ggview)

# Dados ----

## Matriz de composição ----

### Importar ----

comp <- readxl::read_xlsx("matriz_composicao.xlsx")

### Visualizar ----

comp

comp |> dplyr::glimpse()
