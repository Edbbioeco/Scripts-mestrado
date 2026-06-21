# PAcotes ----

library(readxl)

library(tidyverse)

library(sf)

library(terra)

library(terra)

library(ggspatial)

library(ggview)

# Dados ----

## Dados de composição ----

### Importar ----

comp <- readxl::read_xlsx("matriz_composicao.xlsx")

### Visualizar ----

comp

comp |> dplyr::glimpse()
