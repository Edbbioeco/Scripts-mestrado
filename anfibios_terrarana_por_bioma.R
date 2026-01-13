# Pacotes ----

library(readxl)

library(tidyverse)

# Dados ----

## Importando ----

dados_amp <- readxl::read_xlsx("Anuros do Brasil.xlsx")

## Visualizando ----

dados_amp

## Tratando ----

dados_amp <- dados_amp %>%
  dplyr::mutate(Enêmica = dplyr::case_when(Taxa %>% stringr::str_detect("\\*") == TRUE ~ "Sim",
                                           .default = "Não"),
                Taxa = Taxa %>% stringr::str_remove("\\*"),
                Biome = Biome %>% stringr::str_remove_all("\\*")) %>%
  tidyr::separate_rows(Biome, sep = ", ")

dados_amp

dados_amp %>% View()

# Analisando ----

## Número de espécies por bioma ----

n_sps_biomas <- dados_amp %>%
  dplyr::group_by(Biome) %>%
  dplyr::summarise(`Número de Espécies` = n()) %>%
  dplyr::arrange(Biome)

n_sps_biomas

## Calculando as espécies de Terrarana por bioma ----

n_terrarana_biomas <- dados_amp %>%
  dplyr::filter(Family %in% c("Ceuthomantidae",
                              "Brachycephalidae",
                              "Craugastoridae",
                              "Eleutherodactylidae",
                              "Strabomantidae")) %>%
  dplyr::group_by(Biome) %>%
  dplyr::summarise(`Número de Espécies` = n()) %>%
  dplyr::arrange(Biome)

n_terrarana_biomas

## % de Terrarana por bioma ----

terrarana_pocent <- tibble::tibble(Bioma = n_sps_biomas$Biome,
               `Número de Espécies` = n_sps_biomas$`Número de Espécies`,
               `Número de espécies de Terrarana` = n_terrarana_biomas$`Número de Espécies`) %>%
  dplyr::group_by(Bioma) %>%
  dplyr::summarise(`Total de Espécies` = `Número de Espécies` %>% sum(),
                   `Total de Espécies Terrarana` = `Número de espécies de Terrarana` %>% sum(),
                   `% de Espécies Terrarana` =  (`Total de Espécies Terrarana` / `Total de Espécies`) * 100) %>%
  as.data.frame()

terrarana_pocent

## Gráfico ----

terrarana_pocent %>%
  dplyr::mutate(Bioma = dplyr::case_match(Bioma,
                                          "AF" ~ "Mata Atlântica",
                                          "CA" ~ "Caatinga",
                                          "CE" ~ "Cerrado",
                                          "AM" ~ "Amazônia",
                                          "PM" ~ "Pampas",
                                          "PN" ~ "Pantanal")) %>%
  ggplot(aes(`Total de Espécies Terrarana`, `% de Espécies Terrarana`)) +
  geom_point(shape = 21, color = "black", size = 5, aes(fill = Bioma)) +
  labs(x = "Total de espécies de Terrarana no Bioma",
       y = "% de espécies de Terrarana no Bioma") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 120, 20), limits = c(0, 120)) +
  scale_y_continuous(breaks = seq(0, 18, 2), limits = c(0, 18)) +
  scale_fill_manual(values = c("darkgreen", "gold", "orange2", "green2", "cyan4", "brown")) +
  theme_bw() +
  theme(legend.position = "bottom")

terrarana_pocent %>%
  tidyr::pivot_longer(cols = 3:4,
                      names_to = "Tipo",
                      values_to = "Valores") %>%
  dplyr::mutate(Bioma = dplyr::case_match(Bioma,
                                          "AF" ~ "Mata Atlântica",
                                          "CA" ~ "Caatinga",
                                          "CE" ~ "Cerrado",
                                          "AM" ~ "Amazônia",
                                          "PM" ~ "Pampas",
                                          "PN" ~ "Pantanal")) %>%
  ggplot(aes(`Total de Espécies`, Valores, fill = Bioma)) +
  geom_point(shape = 21, color = "black", size = 5) +
  facet_wrap(~ Tipo, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("darkgreen", "gold", "orange2", "green2", "cyan4", "brown")) +
  theme_bw() +
  theme(legend.position = "bottom")

dados_amp %>%
  dplyr::filter(Family %in% c("Ceuthomantidae",
                              "Brachycephalidae",
                              "Craugastoridae",
                              "Eleutherodactylidae",
                              "Strabomantidae")) %>%
  dplyr::filter(Biome == "CA") %>%
  dplyr::pull(Taxa)

dados_amp %>%
  dplyr::filter(Taxa %>% stringr::str_detect("Gastrotheca") == TRUE) %>%
  dplyr::select(Taxa, Family)

plot(1:10, 11:20)
