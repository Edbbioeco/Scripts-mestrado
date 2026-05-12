# Pacotes ----

library(readxl)

library(tidyverse)

library(flextable)

# Dados ----

## Importar ----

comp <- readxl::read_xlsx("levantamento_anuros.xlsx")

## Visualizar ----

comp

comp |> dplyr::glimpse()

# Tabela ----

## Tabela flextable ----

comp |>
  flextable::flextable()
