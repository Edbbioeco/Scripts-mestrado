# Pacotes ----

library(readxl)

library(tidyverse)

library(patchwork)

library(ggview)

# Dados ----

## Importando ----

# Dados ----

## Dados de levantamento ----

### Importando ----

dados <- readxl::read_xlsx("levantamento_anuros.xlsx")

### Visualizando ----

dados

dados |> dplyr::glimpse()

### Tratando ----

dados <- dados |>
  dplyr::mutate(`Segmento da parcelas` = `Segmento da parcelas` |>
                  as.character())

dados |> dplyr::glimpse()

dados

## Variáveis ambientais ----

### Aabertura de dossel e número de poças ----

#### Importando -----

var1 <- readxl::read_xlsx("levantamento_variáveis_ambientais.xlsx")

#### Visualizando -----

var1

var1 |> dplyr::glimpse()

### Altura da serrapilheira ----

#### Importando -----

var2 <- readxl::read_xlsx("levantamento_variáveis_ambientais.xlsx",
                          sheet = 2)

#### Visualizando -----

var2

var2 |> dplyr::glimpse()

### Densidade de Jaqueiras ----

#### Importando -----

var3 <- readxl::read_xlsx("levantamento_variáveis_ambientais.xlsx",
                          sheet = 4)

#### Visualizando -----

var3

var3 |> dplyr::glimpse()

## Distância dos corpos d'água ----

### Importando ----

hid <- readxl::read_xlsx("dados_hidrico.xlsx")

### Visualizando ----

hid

hid |> dplyr::glimpse()

# Gráfico por unidades amostrais ----

## Tratando os dados ----

dados_trat <- dados |>
  dplyr::filter(Ordem == "Anura" &
                  Epípeto != "natalensis" &
                  Gênero != "Frostius" &
                  Família != "Hylidae") |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(`Unidade Amostral`, Espécie, Campanha)) |>
  dplyr::summarise(Abundância = Abundância |> max(),
                   .by = c(`Unidade Amostral`, Espécie)) |>
  dplyr::left_join(dados |>
                     dplyr::distinct(`Unidade Amostral`) |>
                     dplyr::mutate(Amostra = dplyr::row_number()),
                   by = c("Unidade Amostral"))

dados_trat

## Ordem das espécies ----

ordem_especies <- dados_trat |>
  dplyr::summarise(media = sum(Amostra * Abundância) / sum(Abundância),
                   .by = Espécie) |>
  dplyr::arrange(media |> dplyr::desc())

ordem_especies

## Ordem das unidades amostrais ----

ordem_amostras <- dados_trat |>
  dplyr::left_join(ordem_especies,
                   by = "Espécie") |>
  dplyr::summarise(MR = sum(media * Abundância) / sum(Abundância),
                   .by = Amostra) |>
  dplyr::arrange(MR) |>
  dplyr::left_join(dados_trat |>
                     dplyr::select(`Unidade Amostral`, Amostra),
                   by = "Amostra") |>
  dplyr::distinct()

ordem_amostras

## Segundo tratamento ----

dados_tratados <- dados_trat |>
  dplyr::mutate(`Unidade Amostral` = `Unidade Amostral` |>
                  forcats::fct_relevel(ordem_amostras$`Unidade Amostral`),
                Espécie = Espécie |>
                  forcats::fct_relevel(ordem_especies$Espécie))

dados_tratados

## Gráfico ----

dados_tratados |>
  ggplot(aes(`Unidade Amostral`, y = 5, fill = Abundância)) +
  geom_point(shape = 21, color = "black", size = 7.5, stroke = 1, width = 0.75, position = "dodge") +
  facet_grid(Espécie ~ ., scales = "free_y") +
  scale_fill_viridis_c() +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 25,
                                frame.colour = "black",
                                ticks.colour = "black",
                                ticks.linewidth = 0.5)) +
  labs(y = NULL) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(color = "black", size = 15, face = "bold", hjust = 0),
        axis.title = element_text(color = "black", size = 15, face = "bold"),
        panel.spacing = unit(0, "points"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        strip.text.y.right = element_text(angle = 0,
                                          size = 15,
                                          color = "black",
                                          hjust = 0,
                                          face = "bold.italic"),
        legend.position = "top",
        legend.text = element_text(color = "black", size = 15, face = "bold"),
        legend.title = element_text(color = "black", size = 15, face = "bold"),
        panel.background = element_rect(color = "black", linewidth = 1))

ggsave(filename = "grafico_codigo_barras.png", height = 10, width = 12)

# Gráfico para cada variável ----

## Variáveis ----

### Jaqueiras ----

jaqueira <- var3 |>
  dplyr::summarise(Jaqueira = `Densidade de Jaqueiras` |> sum(),
                   .by = `Unidade Amostral`) |>
  tidyr::drop_na() |>
  dplyr::summarise(Jaqueira = Jaqueira |> max(),
                   .by = `Unidade Amostral`)

jaqueira

### Abertura de dossel ----

dossel <- var1 |>
  dplyr::summarise(dossel = `Índice de abertura de dossel` |> mean(),
                   .by = c(`Unidade Amostral`, Campanha)) |>
  tidyr::drop_na() |>
  dplyr::summarise(dossel = dossel |> max(),
                   .by = `Unidade Amostral`)

dossel

### Número de poças ----

numero_pocas <-  var1 |>
  dplyr::summarise(numero = `Número de poças` |> max(),
                   .by = c(`Unidade Amostral`, Campanha)) |>
  tidyr::drop_na() |>
  dplyr::summarise(numero = numero |> max(),
                   .by = `Unidade Amostral`)

numero_pocas

### Altura da serrapilheiras ----

altura <- var2 |>
  dplyr::summarise(altura = Altura |> mean(),
                   .by = c(`Unidade Amostral`, Campanha)) |>
  tidyr::drop_na() |>
  dplyr::summarise(altura = altura |> max(),
                   .by = `Unidade Amostral`)

altura

### Distância dos corpos hídricos ----

hidrico <- hid |>
  dplyr::summarise(Distância = Distância |> mean(),
                   .by = `Unidade Amostral`) |>
  tidyr::drop_na() |>
  dplyr::summarise(Distância = Distância |> max(),
                   .by = `Unidade Amostral`)

hidrico

## Unindo os dados ----

dados_trat2 <- dados_trat |>
  dplyr::left_join(jaqueira,
                   by = "Unidade Amostral") |>
  dplyr::left_join(dossel,
                   by = "Unidade Amostral") |>
  dplyr::left_join(numero_pocas,
                   by = "Unidade Amostral") |>
  dplyr::left_join(altura,
                   by = "Unidade Amostral") |>
  dplyr::left_join(hidrico,
                   by = "Unidade Amostral") |>
  dplyr::rename("Densidade de jaqueiras" = 5,
                "Abertura de dossel" = 6,
                "Número de poças" = 7,
                "Altura de serrapilheira" = 8,
                "Distância de corpos hídricos" = 9)

dados_trat2

dados_trat2 |> dplyr::glimpse()

## Ordem das espécies para cada uma das variáveis ----

ordem_especies <- dados_trat2 |>
  dplyr::summarise(media = sum(`Abertura de dossel` * Abundância) / sum(Abundância),
                   .by = Espécie) |>
  dplyr::arrange(media |> dplyr::desc())

ordem_especies

ordem_especies_var <- function(x){

  message(paste0("Operção para: ", x))

  ordem <- dados_trat2 |>
    dplyr::summarise(media = sum(dados_trat2[[x]] * Abundância) / sum(Abundância),
                     .by = Espécie) |>
    dplyr::arrange(media |> dplyr::desc()) |>
    dplyr::pull(Espécie)

  print(ordem)

  assign(paste0("ordem_sps_", x),
         ordem,
         envir = globalenv())

}

purrr::walk(dados_trat2[5:9] |> names(),
             ordem_especies_var)

## Gerando os gráficos ----

gerar_graficos <- function(x, y){

  paste0("Gráfico para: ", x)

  gg_var <- dados_trat2 |>
    dplyr::mutate(Espécie = Espécie |> forcats::fct_relevel(y)) |>
    ggplot(aes(dados_trat2[[x]], y = 1, fill = Abundância)) +
    geom_point(shape = 21, color = "black", size = 5, stroke = 1, position = "dodge") +
    facet_grid(Espécie ~ ., scales = "free_y", space = "free_y") +
    labs(x = x,
         y = NULL) +
    scale_fill_viridis_c(name = "Abundância",
                         guide = guide_colorbar(title.position = "top",
                                                title.hjust = 0.5,
                                                barwidth = 15,
                                                frame.colour = "black",
                                                frame.linewidth = 1,
                                                ticks.colour = "black",
                                                ticks.linewidth = 1)) +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(color = "black", size = 15, hjust = 0,
                                     face = "bold"),
          axis.title = element_text(color = "black", size = 15,
                                    face = "bold"),
          panel.spacing = unit(0, "points"),
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text.y.right = element_text(angle = 0,
                                            size = 12,
                                            color = "black",
                                            hjust = 0,
                                            face = "bold.italic"),
          legend.position = "top",
          legend.text = element_text(color = "black", size = 15,
                                     face = "bold"),
          legend.title = element_text(color = "black", size = 15,
                                      face = "bold"),
          panel.background = element_rect(color = "black", linewidth = 1))

  print(gg_var)

  assign(paste0("gg_", x),
         gg_var,
         envir = globalenv())

}

lista_ordens <- ls(pattern = "ordem_sps_") |>
  mget(envir = globalenv())

lista_ordens

purrr::walk2(dados_trat2[5:9] |> names(),
             lista_ordens,
             gerar_graficos)

## Gráfico unido ----

gg_nulo <- dados_trat2 |>
  ggplot() +
  theme_void()

(`gg_Densidade de jaqueiras` + `gg_Abertura de dossel`) / (`gg_Altura de serrapilheira` + `gg_Número de poças`) / (`gg_Distância de corpos hídricos` + gg_nulo) +
  ggview::canvas(height = 15, width = 17)

ggsave(filename = "graficos_barras.png", height = 15, width = 17)
