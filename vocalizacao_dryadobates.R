# Pacotes ----

library(tuneR)

library(seewave)

library(tidyverse)

library(patchwork)

# Dados ----

## Importando ----

dryadobates <- tuneR::readWave("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/vocalizações/Dryadobates alagoanus.wav")

## Visualizando ----

dryadobates

dryadobates |> seewave::listen()

# Analisando ----

## Espectrotrograma ----

### Espectrograma inicial ----

gg_spectro <- dryadobates |>
  seewave::ggspectro(wl = 4096,
                     wn = "blackman",
                     ovl = 99,
                     flim = c(5.75, 6.75)) +
  geom_tile(aes(fill = amplitude)) +
  scale_fill_viridis_c(option = "inferno",
                       limits = c(-35, 0),
                       na.value = "transparent") +
  scale_x_continuous(expand = FALSE) +
  scale_y_continuous(expand = FALSE,
                     limits = c(5.75, 6.75)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.text.x = element_blank(),
        axis.title = element_text(color = "black", size = 20),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(color = "black", size = 20,
                                  face = "bold.italic"),
        legend.position = "none",
        plot.margin = margin(0.5, -0.5, 0, 0.5, "cm"),
        panel.background = element_rect(fill = "black"),
        panel.grid = element_line(linetype = "dashed")) +
  ggview::canvas(height = 10, width = 12)

gg_spectro

## Oscilograma ----

### Calculando ----

dryadobates |>
  seewave::oscillo()

tbl_osc <- tibble::tibble(`Time (s)` = seq(0, seewave::duration(dryadobates),
                                           length.out = length(dryadobates@left)),
                          `Amplitude (KU)` = dryadobates@left)

tbl_osc

### ggplot ----

gg_osc <- tbl_osc %>%
  ggplot(aes(`Time (s)`, `Amplitude (KU)`)) +
  geom_line()  +
  scale_x_continuous(expand = FALSE) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        plot.margin = margin(0, -0.5, 0.5, 0.5, "cm"),
        legend.position = "none",
        panel.grid = element_line(linetype = "dashed")) +
  ggview::canvas(height = 10, width = 12)

gg_osc

# Gráfico final ----

(gg_spectro / gg_osc) &
    ggview::canvas(height = 10, width = 12)

ggsave(filename = "vocalizacoes_dryadobates.png", height = 10, width = 12)
