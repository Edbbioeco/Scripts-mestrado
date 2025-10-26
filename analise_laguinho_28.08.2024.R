# Pacotes ----

library(tidyverse)

library(readxl)

library(hillR)

library(iNEXT)

# Dados ----

## Importando ----

edson <- readxl::read_xlsx("dados_levantamento_28.08.2024.xlsx",
                           sheet = 1)

layla <- readxl::read_xlsx("dados_levantamento_28.08.2024.xlsx",
                           sheet = 2)

## Visualizando e checando ----

edson

edson %>% dplyr::glimpse()

layla

layla %>% dplyr::glimpse()

# Análises ----

## Número de espécies ----

### Edson ----

edson %>%
  dplyr::summarise(`Numero de espécies` = Espécie %>% dplyr::n_distinct())

### Layla ----

layla %>%
  dplyr::summarise(`Numero de espécies` = Espécie %>% dplyr::n_distinct())

## Abundância das espécies ----

### Edson ----

edson_abundancia <- edson %>%
  dplyr::summarise(Abundância = dplyr::n(), .by = Espécie) %>%
  dplyr::arrange(Abundância %>% dplyr::desc())

edson_abundancia

### Layla ----

layla_abundancia <- layla %>%
  dplyr::summarise(Abundância = dplyr::n(), .by = Espécie) %>%
  dplyr::arrange(Abundância %>% dplyr::desc())

layla_abundancia

### Total ----

abundancia_total <- dplyr::bind_rows(edson_abundancia, layla_abundancia) %>%
  dplyr::summarise(Abundância = Abundância %>% max(na.rm = TRUE), .by = Espécie) %>%
  dplyr::arrange(Abundância %>% dplyr::desc())

abundancia_total

## Diversidade ----

### Índice de Hill ----

#### Edson ----

edson_hill <- edson %>%
  dplyr::summarise(`Abundância acumulada` = dplyr::n(), .by = Espécie) %>%
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = `Abundância acumulada`)

edson_hill %>%
  hillR::hill_taxa(q = 1)

edson_hill %>%
  hillR::hill_taxa(q = 2)

#### Layla ----

layla_hill <- layla %>%
  dplyr::summarise(`Abundância acumulada` = dplyr::n(), .by = Espécie) %>%
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = `Abundância acumulada`)

layla_hill %>%
  hillR::hill_taxa(q = 1)

layla_hill %>%
  hillR::hill_taxa(q = 2)

#### Total ----

abundancia_total_hill <- abundancia_total %>%
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Abundância)

abundancia_total_hill %>%
  hillR::hill_taxa(q = 1)

abundancia_total_hill %>%
  hillR::hill_taxa(q = 2)

### Diagrama de Whittaker ----

#### Edson ----

edson_whittaker <- edson %>%
  dplyr::summarise(`Abundância acumulada` = dplyr::n(), .by = Espécie) %>%
  dplyr::arrange(`Abundância acumulada` %>% dplyr::desc()) %>%
  dplyr::mutate(Rank = 1:10 %>% as.integer())

edson_whittaker

edson_whittaker %>%
  ggplot(aes(Rank, `Abundância acumulada`, group = 1)) +
  geom_line() +
  geom_point(shape = 21, color = "black", size = 2.5, fill = "gold") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  theme_bw()

#### Layla ----

layla_whittaker <- layla %>%
  dplyr::summarise(`Abundância acumulada` = dplyr::n(), .by = Espécie) %>%
  dplyr::arrange(`Abundância acumulada` %>% dplyr::desc()) %>%
  dplyr::mutate(Rank = 1:6 %>% as.integer())

layla_whittaker

layla_whittaker %>%
  ggplot(aes(Rank, `Abundância acumulada`, group = 1)) +
  geom_line() +
  geom_point(shape = 21, color = "black", size = 2.5, fill = "gold") +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  theme_bw()

#### Total ----

abundancia_total_whittaker <- abundancia_total %>%
  dplyr::mutate(`Abundância acumulada` = Abundância,
                Rank = 1:11 %>% as.integer())

abundancia_total_whittaker

abundancia_total_whittaker %>%
  ggplot(aes(Rank, `Abundância acumulada`, group = 1)) +
  geom_line() +
  geom_point(shape = 21, color = "black", size = 2.5, fill = "gold") +
  scale_x_continuous(breaks = seq(1, 11, 1)) +
  theme_bw()

## Curva de rarefação ----

### Edson ----

edson_rare <- edson %>%
  dplyr::summarise(`Abundância acumulada` = dplyr::n(), .by = Espécie) %>%
  dplyr::mutate(rowname = Espécie) %>%
  tibble::column_to_rownames() %>%
  as.data.frame() %>%
  dplyr::select(2) %>%
  iNEXT::iNEXT(q = 0, datatype = "abundance", endpoint = 81)

edson_rare %>%
  iNEXT::ggiNEXT() +
  scale_linetype_discrete(labels = c("Interpolado", "Extrapolado")) +
  labs(x = "Número de indivíduos", y = " Riqueza de espécies") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"))

### Layla ----

layla_rare <- layla %>%
  dplyr::summarise(`Abundância acumulada` = dplyr::n(), .by = Espécie) %>%
  dplyr::mutate(rowname = Espécie) %>%
  tibble::column_to_rownames() %>%
  as.data.frame() %>%
  dplyr::select(2) %>%
  iNEXT::iNEXT(q = 0, datatype = "abundance", endpoint = 45)

layla_rare %>%
  iNEXT::ggiNEXT() +
  scale_linetype_discrete(labels = c("Interpolado", "Extrapolado")) +
  labs(x = "Número de indivíduos", y = " Riqueza de espécies") +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  theme_bw() +
  theme(legend.position = "bottom")

### Total ----

abundancia_total_rare <- abundancia_total %>%
  dplyr::mutate(rowname = Espécie) %>%
  tibble::column_to_rownames() %>%
  as.data.frame() %>%
  dplyr::select(2) %>%
  iNEXT::iNEXT(q = 0, datatype = "abundance", endpoint = 80)

abundancia_total_rare %>%
  iNEXT::ggiNEXT() +
  scale_linetype_discrete(labels = c("Interpolado", "Extrapolado")) +
  labs(x = "Número de indivíduos", y = " Riqueza de espécies") +
  scale_y_continuous(breaks = seq(0, 25, 5)) +
  theme_bw() +
  theme(legend.position = "bottom")

## Tipo de substrato ----

### Por espécie ----

#### Edson ----

edson_sp_sub <- edson %>%
  dplyr::summarise(Contagem = dplyr::n(), .by = c(Espécie, Substrato))

edson_sp_sub

edson_sp_sub %>%
  ggplot(aes(Espécie, Contagem, fill = Substrato)) +
  geom_col(color = "black", position = "dodge") +
  theme_bw() +
  labs(x = NULL) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, color = "black", face = "italic"),
        legend.position = "bottom")

#### Layla ----

layla_sp_sub <- layla %>%
  dplyr::summarise(Contagem = dplyr::n(), .by = c(Espécie, Substrato))

layla_sp_sub

layla_sp_sub %>%
  ggplot(aes(Espécie, Contagem, fill = Substrato)) +
  geom_col(color = "black", position = "dodge") +
  theme_bw() +
  labs(x = NULL) +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, color = "black", face = "italic"),
        legend.position = "bottom")

### Por indivíduo ----

#### Edson ----

edson_ind_sub <- edson %>%
  dplyr::summarise(Contagem = dplyr::n(), .by = Substrato)

edson_ind_sub

edson_ind_sub %>%
  ggplot(aes(Substrato, Contagem, fill = Substrato)) +
  geom_col(color = "black", position = "dodge") +
  theme_bw() +
  labs(x = NULL) +
  theme(axis.text.x = element_text(color = "black"),
        legend.position = "bottom")

#### Layla ----

layla_ind_sub <- layla %>%
  dplyr::summarise(Contagem = dplyr::n(), .by = Substrato)

layla_ind_sub

layla_ind_sub %>%
  ggplot(aes(Substrato, Contagem, fill = Substrato)) +
  geom_col(color = "black", position = "dodge") +
  theme_bw() +
  labs(x = NULL) +
  scale_y_continuous(breaks = seq(0, 20, 5)) +
  theme(axis.text.x = element_text(color = "black"),
        legend.position = "bottom")

## Série temporal ----

### Número de Espécies ----

#### Edson ----

edson_horario <- edson %>%
  dplyr::mutate(Horário = Horário %>% lubridate::as_datetime(),
                Intervalo = Horário %>% lubridate::floor_date(unit = "5 minutes")) %>%
  dplyr::summarise(Contagem = Espécie %>% dplyr::n_distinct(), .by = Intervalo)

edson_horario %>%
  ggplot(aes(Intervalo, Contagem)) +
  geom_line() +
  geom_point(shape = 21, color = "black", size = 2.5, fill = "gold") +
  labs(x = "Horário",
       y = "Número de Espécies") +
  scale_y_continuous(breaks = seq(1, 10, 1)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"))

#### Layla ----

layla_horario <- layla %>%
  dplyr::mutate(Horário = Horário %>% lubridate::as_datetime(),
                Intervalo = Horário %>% lubridate::floor_date(unit = "5 minutes")) %>%
  dplyr::summarise(Contagem = Espécie %>% dplyr::n_distinct(), .by = Intervalo)

layla_horario %>%
  ggplot(aes(Intervalo, Contagem)) +
  geom_line() +
  geom_point(shape = 21, color = "black", size = 2.5, fill = "gold") +
  labs(x = "Horário",
       y = "Número de Espécies") +
  scale_y_continuous(breaks = seq(1, 10, 1)) +
  theme_bw()

#### Total ----

total_horario <- dplyr::bind_rows(edson, layla) %>%
  dplyr::mutate(Horário = Horário %>% lubridate::as_datetime(),
                Intervalo = Horário %>% lubridate::floor_date(unit = "5 minutes")) %>%
  dplyr::summarise(Contagem = Espécie %>% dplyr::n_distinct(), .by = Intervalo) %>%
  dplyr::bind_rows(edson_horario, layla_horario) %>%
  dplyr::mutate(ID = rep(c("Total", "Edson", "Layla"), times = c(14, 14, 12)))

total_horario %>%
  ggplot(aes(Intervalo, Contagem, fill = ID, color = ID)) +
  geom_line() +
  geom_point(shape = 21, color = "black", size = 2.5) +
  labs(x = "Horário",
       y = "Número de Espécies") +
  scale_y_continuous(breaks = seq(1, 10, 1)) +
  theme_bw() +
  theme(legend.position = "bottom")

### Abundância ----

#### Edson ----

edson_horario_ind <- edson %>%
  dplyr::mutate(Horário = Horário %>% lubridate::as_datetime(),
                Intervalo = Horário %>% lubridate::floor_date(unit = "5 minutes")) %>%
  dplyr::summarise(Contagem = dplyr::n(), .by = Intervalo)

edson_horario_ind %>%
  ggplot(aes(Intervalo, Contagem)) +
  geom_line() +
  geom_point(shape = 21, color = "black", size = 2.5, fill = "gold") +
  labs(x = "Horário",
       y = "Número de indivíduos") +
  scale_y_continuous(breaks = seq(1, 10, 1)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"))

#### Layla ----

layla_horario_ind <- layla %>%
  dplyr::mutate(Horário = Horário %>% lubridate::as_datetime(),
                Intervalo = Horário %>% lubridate::floor_date(unit = "5 minutes")) %>%
  dplyr::summarise(Contagem = dplyr::n(), .by = Intervalo)

layla_horario_ind %>%
  ggplot(aes(Intervalo, Contagem)) +
  geom_line() +
  geom_point(shape = 21, color = "black", size = 2.5, fill = "gold") +
  labs(x = "Horário",
       y = "Número de indivíduos") +
  scale_y_continuous(breaks = seq(1, 10, 1)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"))

#### Total ----

total_horario_ind <- dplyr::bind_rows(edson, layla) %>%
  dplyr::mutate(Horário = Horário %>% lubridate::as_datetime(),
                Intervalo = Horário %>% lubridate::floor_date(unit = "5 minutes")) %>%
  dplyr::summarise(Contagem = dplyr::n(), .by = Intervalo) %>%
  dplyr::bind_rows(edson_horario_ind, layla_horario_ind) %>%
  dplyr::mutate(ID = rep(c("Total", "Edson", "Layla"), times = c(14, 14, 12)))

total_horario_ind %>%
  ggplot(aes(Intervalo, Contagem, fill = ID, color = ID)) +
  geom_line() +
  geom_point(shape = 21, color = "black", size = 2.5) +
  labs(x = "Horário",
       y = "Número de indivíduos") +
  scale_y_continuous(breaks = seq(1, 11, 1)) +
  theme_bw() +
  theme(legend.position = "bottom")
