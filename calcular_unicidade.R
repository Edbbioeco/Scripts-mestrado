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
