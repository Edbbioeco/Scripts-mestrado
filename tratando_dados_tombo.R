# Pacotes ----

library(readxl)

library(tidyverse)

library(writexl)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("dados_pis_saltinho.xlsx")

## Visualizando ----

dados

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
  dplyr::mutate(Data = Data |> lubridate::as_date(),
                CRC = dplyr::case_when(!CRC |> is.na() ~ paste0(CRC,
                                                                " mm"),
                                       .default = CRC |> as.character()),
                CHUFPE = c(CHUFPE |> na.omit(),
                           paste0("A-",
                                  2583:2593)),
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
  as.data.frame()

dados_trat

## Exportando ----

dados_trat |>
  writexl::write_xlsx("dados_tratados_pis.xlsx")
