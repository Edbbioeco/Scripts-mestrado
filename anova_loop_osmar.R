# Pacotes ----

library(readxl)

library(tidyverse)

library(sjPlot)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("ambientais_comgeo.xlsx")

## Visualizando ----

dados

## Checando ----

dados %>% dplyr::glimpse()

## Tratando ----

dados <- dados %>%
  dplyr::mutate(dplyr::across(3:29, as.numeric),
                dplyr::across(1:2, as.character))

dados

dados %>% dplyr::glimpse()

# loop ----

## Criando a função do loop ----

loop_anova <- function(x){

  stringr::str_glue("# modelo para a variável {x}") %>% message()

  anova <- aov(dados[[x]] ~ cluster, data = dados)

  message("## normalidade dos resíduos")

  anova %>%
    residuals() %>%
    shapiro.test() %>%
    print()

  message("## homogeneidade de variância dos resíduos")

  bartlett.test(dados[[x]] ~ cluster, data = dados) %>%
    print()

  message("## Estatísticas do modelo")

  anova %>%
    summary() %>%
    print()

  message(" ")

}

## Rodando o loop ----

purrr::walk(dados[5:29] %>% names, loop_anova)
