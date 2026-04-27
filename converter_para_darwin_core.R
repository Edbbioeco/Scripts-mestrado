# Pacotes ----

library(readxl)

library(tidyverse)

library(writexl)

# Dados ----

## Modelo Darwin Core ----

### Importar ----

darwin_core <- readxl::read_xlsx("Template_lista_especies.xlsx")

### Visualizar ----

darwin_core

darwin_core |> dplyr::glimpse()

## Dados do levantamento de anuros ----

### Importar ----

anuros <- readxl::read_xlsx("levantamento_anuros.xlsx")

### Visualizar ----

anuros

anuros |> dplyr::glimpse()
