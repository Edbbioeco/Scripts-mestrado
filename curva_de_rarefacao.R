# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggview)

# Dados ----

## Importar ----

comp <- readxl::read_xlsx("levantamento_anuros.xlsx")

## Visualizar ----

comp

comp |> dplyr::glimpse()

## Tratar ----

comp_trat <- comp |>
  dplyr::filter(Ordem == "Anura" &
                  !Epípeto == "natalensis" &
                  Gênero != "Frostius" &
                  Família != "Hylidae" &
                  `Unidade Amostral` != "T1P1") |>
  dplyr::summarise(Abundância = Abundância |> max(),
                   .by = c(Espécie, `Unidade Amostral`, Campanha)) |>
  dplyr::mutate(`Unidade Amostral` = paste0(Campanha, " ", `Unidade Amostral`)) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Abundância,
                     values_fill = 0)

comp_trat

# Curva de rarefação ----

## Calcular curva de rarefação ----

chao_df <- comp_trat |>
  dplyr::select(dplyr::where(is.numeric)) |>
  vegan::estaccumR() |>
  summary(display = c("S", "chao")) %>%
  .[[1]] |>
  as.data.frame() |>
  dplyr::rename("Sampling record" = N,
                "Richness" = S) |>
  dplyr::mutate(Tipo = "Observed") |>
  dplyr::bind_rows(comp_trat |>
                     dplyr::select(dplyr::where(is.numeric)) |>
                     vegan::estaccumR() |>
                     summary(display = c("S", "chao")) %>%
                     .[[2]] |>
                     as.data.frame() |>
                     dplyr::rename("Sampling record" = N,
                                   "Richness" = Chao) |>
                     dplyr::mutate(Tipo = "Estimated"))

chao_df

## Gráfico ----

chao_df |>
  ggplot(aes(`Sampling record`, Richness, color = Tipo, fill = Tipo)) +
  geom_ribbon(aes(ymax = Richness + Std.Dev,
                  ymin = Richness - Std.Dev),
              alpha = 0.3,
              linewidth = 0) +
  geom_line(linewidth = 1) +
  geom_point(shape = 21, size = 5, color = "black", stroke = 1) +
  labs(fill = NULL,
       color = NULL) +
  scale_x_continuous(breaks = seq(0, 32, 2)) +
  scale_y_continuous(breaks = seq(0, 14, 1),
                     limits = c(0, 14)) +
  scale_color_manual(values = c("royalblue", "orange")) +
  scale_fill_manual(values = c("royalblue", "orange")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 17.5),
        axis.title = element_text(color = "black", size = 17.5),
        legend.text = element_text(color = "black", size = 17.5),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 20),
        legend.position = "bottom",
        title = element_text(color = "black", size = 17.5),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10,
                 width = 12)

ggsave(filename = "curva_de_rarefacao.png",
       height = 10,
       width = 12)
