# Pacorws ----

library(tidyverse)

library(readxl)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("G:/Meu Drive/UFPE/projeto mestrado/parcelas_saltinho.xlsx")

## Visualizando ----

dados

# Gráficos ----

## Ordem dos segmentos ----

ordem <- dados %>%
  dplyr::filter(Segmento != "Trilha-0") %>%
  dplyr::pull(Segmento) %>%
  unique()

ordem

## Histograma ----

dados %>%
  dplyr::select(1:9) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(Trilha = paste0("Trilha ", Trilha),
                Parcela = paste0("Parcela ", Parcela)) %>%
  tidyr::unite(col = "Trilha-Parcela",
               sep = "-",
               Trilha:Parcela) %>%
  ggplot(aes(`Azimute (graus)`, fill = `Trilha-Parcela`)) +
  geom_histogram(color = "black") +
  scale_fill_viridis_d() +
  facet_wrap(~`Trilha-Parcela`,
             ncol = 2) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        legend.position = "none")

ggsave(filename = "grafico_azimute_histograma.png", height = 12, width = 14)

## Linhas e pontos ----

dados %>%
  dplyr::select(1:9) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(Trilha = paste0("Trilha ", Trilha),
                Parcela = paste0("Parcela ", Parcela)) %>%
  tidyr::unite(col = "Trilha-Parcela",
               sep = "-",
               Trilha:Parcela) %>%
  dplyr::mutate(Segmento = Segmento %>% forcats::fct_relevel(ordem)) %>%
  ggplot(aes(`Azimute (graus)`, Segmento, fill = `Trilha-Parcela`, color = `Trilha-Parcela`, group = 1)) +
  geom_line() +
  geom_point(shape = 21, color = "black") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(0, 360, 45),
                     limits = c(0, 360)) +
  facet_wrap(~`Trilha-Parcela`,
             ncol = 3) +
  coord_radial(inner.radius = 0, start = 6) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.title = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        legend.position = "none")

ggsave(filename = "grafico_azimute_circular.png", height = 12, width = 14)
