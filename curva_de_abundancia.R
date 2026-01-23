# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

# Dados ----

## Espécies -----

### Importando ----

especies <- readxl::read_xlsx("levantamento_anuros.xlsx")

### Visualizando ----

especies

especies |> dplyr::glimpse()

### Tratando ----

especies <- especies |>
  dplyr::mutate(`Segmento da parcelas` = `Segmento da parcelas` |>
                  as.character())

especies |> dplyr::glimpse()

## Riqueza e diversidade alfa -----

### Criando a matriz ----

especies_alfa <- especies |>
  dplyr::filter(Ordem == "Anura" &
                  !Epípeto == "natalensis" &
                  Gênero != "Frostius" &
                  Família != "Hylidae") |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(`Unidade Amostral`, Espécie, Campanha)) |>
  dplyr::summarise(Abundância = Abundância |> max(),
                   .by = c(`Unidade Amostral`, Espécie)) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Abundância,
                     values_fill = 0)

especies_alfa

nomes_linhas <- especies_alfa |>
  dplyr::pull(`Unidade Amostral`)

nomes_linhas

especies_alfa <- especies_alfa |>
  dplyr::select(-`Unidade Amostral`)

rownames(especies_alfa) <- nomes_linhas

especies_alfa |> names()

especies_alfa |> rownames()

especies_alfa |> dplyr::glimpse()

# Curva de abundância ----

## Objeto com os modelos ----

modelos_curva <- especies_alfa |> vegan::radfit()

modelos_curva

## Parcelas ----

parcelas <- modelos_curva |> names()

parcelas

## Loop ----

curva_comunidades <- function(parcelas){

  curva_df <- modelos_curva[[parcelas]] |>
    AIC() |>
    data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::rename("Modelo" = 1,
                  "AIC" = 2) |>
    dplyr::mutate(`Unidade Amostral` = parcelas) |>
    dplyr::slice_min(AIC,
                     n = 1)

  assign(paste0("df_curva_", parcelas),
         curva_df,
         envir = globalenv())

}

purrr::walk(modelos_curva |> names(),
            curva_comunidades)

modelos_df_curva <- ls(pattern = "df_curva_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

modelos_df_curva

## Gráfico ----

especies_alfa |>
  dplyr::mutate(`Unidade Amostral` = especies_alfa |> rownames()) |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "Espécie",
                      values_to = "Abundância") |>
  dplyr::filter(Abundância > 0) |>
  dplyr::arrange(`Unidade Amostral`, Abundância |> dplyr::desc()) |>
  dplyr::mutate(Rank = dplyr::row_number() |> forcats::as_factor(),
                .by = `Unidade Amostral`) |>
  dplyr::left_join(modelos_df_curva,
                   by = "Unidade Amostral") |>
  dplyr::mutate(AIC = AIC |> round(2)) |>
  tidyr::unite(col = "Unidade Amostral",
               c(1, 5),
               sep = ": ") |>
  tidyr::unite(col = "Unidade Amostral",
               c(1, 5),
               sep = " = ") |>
  ggplot(aes(Rank, Abundância, group = 1)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3.5) +
  facet_wrap(~`Unidade Amostral`, scales = "free_x") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 15),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "none",
        title = element_text(color = "black", size = 15),
        panel.background = element_rect(color = "black", linewidth = 1))

