# Pacotes ----

library(readxl)

library(tidyverse)

library(betapart)

library(vegan)

library(performance)

library(broom)

library(flextable)

library(ggview)

library(reshape2)

library(glmmTMB)

library(DHARMa)

# Dados ----

## Abundância de espécies ----

### Importar ----

especies <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/matriz_composicao.xlsx")

### Visualizar ----

especies

especies |> dplyr::glimpse()

## Variáveis ambientais ----

### Importar ----

ambientais <-readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/dados/matriz_ambientais.xlsx")

### Visualizar ----

ambientais

ambientais |> dplyr::glimpse()

# Data frame dos dados do modelo ----

## Dissimilaridade da composição ----

### Impportar funções ----

source("https://raw.githubusercontent.com/Edbbioeco/Scripts-mestrado/main/ruggiero_indices.R")
