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

dryadobates |>
  seewave::ggspectro(f = 512) +
  stat_contour(geom = "polygon",
               aes(fill = ..level..,
                   alpha = ..level..),
               bins = 150) +
  scale_fill_viridis_c(option = "inferno",
                       direction = -1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

# Analisando ----

## Espectrotrograma ----

### Espectrotro ----

spectro <- dryadobates |>
  seewave::spectro(wl = 512,
                   ovl = 99,
                   flim = c(5.75, 6.75),
                   tlim = c(0.85, 1.8))

### ggplot ----

tibl_spectro <- tibble::tibble(`Time (s)` = rep(spectro$time,
                                                each = length(spectro$freq)),
                               `Frequency (KHz)` = rep(spectro$freq,
                                                       length(spectro$time)),
                               Amplitude = spectro$amp |>
                                 as.vector()) |>
  dplyr::mutate(`Time (s)` = `Time (s)` - 0.85) |>
  dplyr::filter(Amplitude > -35)

tibl_spectro

gg_spectro <- tibl_spectro |>
  ggplot(aes(`Time (s)`, `Frequency (KHz)`, z = Amplitude)) +
  stat_contour(geom = "polygon",
               aes(fill = ..level..),
               bins = 150) +
  scale_fill_viridis_c(option = "inferno") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
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

## Densidade de frequência ----

spec <- dryadobates |>
  seewave::meanspec(from = 0.85,
                    to = 1.8,
                    dB = "C")

tbl_m_spec <- spec |>
  tibble::as_tibble() |>
  dplyr::rename("Frequency (KHz)" = x,
                "Amplitude density (dB/KHz)" = y) |>
  dplyr::filter(`Frequency (KHz)`  |> dplyr::between(5.75, 6.75) &
                  `Amplitude density (dB/KHz)` >= -32.5)

tbl_m_spec

### ggplot ----

gg_m_spec <- tbl_m_spec |>
  ggplot(aes(`Frequency (KHz)`, `Amplitude density (dB/KHz)`)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(5.75, 6.75)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-32.5, 1),
                     position = "right") +
  coord_flip() +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.text.y = element_blank(),
        axis.title = element_text(color = "black", size = 12),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(color = "black", size = 12,
                                  face = "bold.italic"),
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0, -0.5, "cm"),
        panel.grid = element_line(linetype = "dashed"))

gg_m_spec

## Oscilograma ----

### Calculando ----

dryadobates |>
  seewave::oscillo()

tbl_osc <- tibble::tibble(`Time (s)` = seq(0, seewave::duration(dryadobates),
                                           length.out = length(dryadobates@left)),
                          `Amplitude (KU)` = dryadobates@left) |>
  dplyr::filter(`Time (s)` |> dplyr::between(0.85, 1.8)) |>
  dplyr::mutate(`Time (s)` = `Time (s)` - 0.85)

tbl_osc

### ggplot ----

gg_osc <- tbl_osc %>%
  ggplot(aes(`Time (s)`, `Amplitude (KU)`)) +
  geom_line()  +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 12),
        plot.margin = margin(0, -0.5, 0.5, 0.5, "cm"),
        legend.position = "none",
        panel.grid = element_line(linetype = "dashed"))

gg_osc

# Gráfico final ----

gg_spectro + gg_m_spec + gg_osc +
  patchwork::plot_layout(ncol = 2)

ggsave(filename = "vocalizacoes_dryadobates.png", height = 10, width = 12)
