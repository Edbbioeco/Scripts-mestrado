# Pacotes ----

library(tidyverse)

library(readxl)

library(hillR)

library(iNEXT)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("trilha_saltinho_praticas.xlsx",
                           sheet = 1)

## Visualizando e checando ----

dados

# Análises ----

## Número de espécies ----

num_esp <- dados %>%
  dplyr::summarise(`Numero de espécies` = Espécie %>% dplyr::n_distinct()) %>%
  dplyr::pull()

num_esp

## Abundância das espécies ----

dados_abundancia <- dados %>%
  dplyr::summarise(Abundância = dplyr::n(), .by = Espécie) %>%
  dplyr::arrange(Abundância %>% dplyr::desc())

dados_abundancia

## Diversidade ----

### Índice de Hill ----

dados_hill <- dados %>%
  dplyr::summarise(`Abundância acumulada` = dplyr::n(), .by = Espécie) %>%
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = `Abundância acumulada`)

dados_hill %>%
  hillR::hill_taxa(q = 1)

dados_hill %>%
  hillR::hill_taxa(q = 2)

### Diagrama de Whittaker ----

dados_whittaker <- dados %>%
  dplyr::summarise(`Abundância acumulada` = dplyr::n(), .by = Espécie) %>%
  dplyr::arrange(`Abundância acumulada` %>% dplyr::desc()) %>%
  dplyr::mutate(Rank = 1:num_esp %>% as.integer())

dados_whittaker

dados_whittaker %>%
  ggplot(aes(Rank, `Abundância acumulada`, group = 1)) +
  geom_line() +
  geom_point(shape = 21, color = "black", size = 2.5, fill = "gold") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  theme_bw()

## Curva de rarefação ----

dados_rare <- dados %>%
  dplyr::summarise(`Abundância acumulada` = dplyr::n(), .by = Espécie) %>%
  dplyr::mutate(rowname = Espécie) %>%
  tibble::column_to_rownames() %>%
  as.data.frame() %>%
  dplyr::select(2) %>%
  iNEXT::iNEXT(q = 0, datatype = "abundance", endpoint = 75)

dados_rare %>%
  iNEXT::ggiNEXT() +
  scale_linetype_discrete(labels = c("Interpolado", "Extrapolado")) +
  labs(x = "Número de indivíduos", y = " Riqueza de espécies") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"))

## Tipo de substrato ----

### Por espécie ----

dados_sp_sub <- dados %>%
  dplyr::summarise(Contagem = dplyr::n(), .by = c(Espécie, Substrato))

dados_sp_sub

dados_sp_sub %>%
  ggplot(aes(Espécie, Contagem, fill = Substrato)) +
  geom_col(color = "black", position = "dodge") +
  theme_bw() +
  labs(x = NULL) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, color = "black", face = "italic"),
        legend.position = "bottom")

### Por indivíduo ----

dados_ind_sub <- dados %>%
  dplyr::summarise(Contagem = dplyr::n(), .by = Substrato)

dados_ind_sub

dados_ind_sub %>%
  ggplot(aes(Substrato, Contagem, fill = Substrato)) +
  geom_col(color = "black", position = "dodge") +
  theme_bw() +
  labs(x = NULL) +
  theme(axis.text.x = element_text(color = "black"),
        legend.position = "bottom")

## Série temporal ----

### Número de Espécies ----

#### Edson ----

dados_horario <- dados %>%
  dplyr::mutate(Horário = Horário %>% lubridate::as_datetime(),
                Intervalo = Horário %>% lubridate::floor_date(unit = "5 minutes")) %>%
  dplyr::summarise(Contagem = Espécie %>% dplyr::n_distinct(), .by = Intervalo)

dados_horario %>%
  ggplot(aes(Intervalo, Contagem)) +
  geom_line() +
  labs(x = "Horário",
       y = "Número de Espécies") +
  scale_y_continuous(breaks = seq(1, 10, 1)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"))

### Abundância ----

#### Edson ----

dados_horario_ind <- dados %>%
  dplyr::mutate(Horário = Horário %>% lubridate::as_datetime(),
                Intervalo = Horário %>% lubridate::floor_date(unit = "5 minutes")) %>%
  dplyr::summarise(Contagem = dplyr::n(), .by = Intervalo)

dados_horario_ind %>%
  ggplot(aes(Intervalo, Contagem)) +
  geom_line() +
  labs(x = "Horário",
       y = "Número de indivíduos") +
  scale_y_continuous(breaks = seq(1, 10, 1)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"))
