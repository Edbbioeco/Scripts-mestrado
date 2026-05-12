# PAcotes ----

library(readxl)

library(tidyverse)

library(flextable)

# Dados ----

## Importar ----

comp <- readxl::read_xlsx("matriz_composicao.xlsx")

## Visualizar ----

comp

comp |> dplyr::glimpse()
