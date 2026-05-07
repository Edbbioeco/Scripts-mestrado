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

### Espectrotro ----

dryadobates |>
  seewave::spectro(wl = 2048,
                   wn = "blackman",
                   ovl = 99,
                   flim = c(5.75, 6.75))

### ggplot ----

tibl_spectro <- tibble::tibble(`Time (s)` = rep(spectro$time,
                                                each = length(spectro$freq)),
                               `Frequency (KHz)` = rep(spectro$freq,
                                                       length(spectro$time)),
                               Amplitude = spectro$amp |>
                                 as.vector()) |>
  dplyr::filter(Amplitude > -35)

tibl_spectro

gg_spectro <- tibl_spectro |>
  ggplot(aes(`Time (s)`, `Frequency (KHz)`, z = Amplitude)) +
  stat_contour(geom = "polygon",
               aes(fill = ..level..),
               bins = 150) +
  scale_fill_viridis_c(option = "inferno") +
  scale_x_continuous(expand = FALSE) +
  scale_y_continuous(expand = FALSE,
                     limits = c(5.75, 6.75)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.text.x = element_blank(),
        axis.title = element_text(color = "black", size = 12),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(color = "black", size = 12,
                                  face = "bold.italic"),
        legend.position = "none",
        plot.margin = margin(0.5, -0.5, 0, 0.5, "cm"),
        panel.background = element_rect(fill = "black"),
        panel.grid = element_line(linetype = "dashed"))

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
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 12),
        plot.margin = margin(0, -0.5, 0.5, 0.5, "cm"),
        legend.position = "none",
        panel.grid = element_line(linetype = "dashed"))

  gg_osc

# Gráfico final ----

(gg_spectro / gg_osc) &
    ggview::canvas(height = 10, width = 12)

ggsave(filename = "vocalizacoes_dryadobates.png", height = 10, width = 12)
