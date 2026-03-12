# Pacotes ----

library(readxl)

library(tidyverse)

library(writexl)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("dados_pis_saltinho.xlsx")

## Visualizando ----

dados

dados |> as.data.frame()

dados |> dplyr::glimpse()

# Tratando ----

## Criando um dataframe com os dados de PIS completos ----

dados_pis <- tibble::tibble(ID = paste0("0", 566:603))

dados_pis

dados_pis |> dplyr::glimpse()

## Unindo os dataframes ----

dados_trat <- dados |>
  dplyr::right_join(dados_pis,
                    by = "ID") |>
  dplyr::filter(ID != "0586") |>
  dplyr::mutate(Data = Data |> lubridate::as_date(),
                Data = dplyr:::case_when(ID %in% c("0579",
                                                   "0580",
                                                   "0581") ~ "2025-08-20" |>
                                           lubridate::as_date(),
                                         ID %in% c("0582",
                                                   "0583",
                                                   "0584",
                                                   "0585",
                                                   "0591",
                                                   "0592",
                                                   "0593") ~ "2025-09-25" |>
                                           lubridate::as_date(),
                                        .default = Data),
                CRC = dplyr::case_when(!CRC |> is.na() ~ paste0(CRC,
                                                                " mm"),
                                       .default = CRC |> as.character()) |>
                  stringr::str_replace("\\.", ","),
                CHUFPE = c(CHUFPE |> na.omit(),
                           paste0("A-",
                                  2583:2592)),
                Família = dplyr::if_else(Família |> is.na(),
                                         "Leptodactylidae",
                                         Família),
                Gênero = dplyr::if_else(Gênero |> is.na(),
                                         "Physalaemus",
                                         Gênero),
                Espécie = dplyr::if_else(Espécie |> is.na(),
                                         paste(Gênero, " cuvieri"),
                                         Espécie)) |>
  tidyr::fill(Data) |>
  dplyr::mutate(Dia = Data |> lubridate::day(),
                Mês = Data |> lubridate::month() |>
                  as.roman() |>
                  stringr::str_to_lower(),
                Ano = Data |> lubridate::year(),
                Data = paste0(Dia,
                              ".",
                              Mês,
                              ".",
                              Ano)) |>
  dplyr::select(-c(Dia, Mês, Ano)) |>
  as.data.frame()

dados_trat

## Exportando ----

dados_trat |>
  writexl::write_xlsx("dados_tratados_pis.xlsx")
