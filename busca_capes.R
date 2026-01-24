# Pacotes ----

library(capesR)

library(tidyverse)

# Dados ----

## Baixar os arquivos do repositório da CAPES/OSF ----

arquivos_capes <- capesR::download_capes_data(years = 2000:2025, ### períodos desejado ----
                                              destination = "dados_capes") ### nome do diretório que os dados ficarão ----

arquivos_capes

## Transformar a lista nomeada em vetor simples de caminhos ----

arquivos_parquet <- arquivos_capes |> unlist()

arquivos_parquet

## Ler os dados completos ----

dados_completos <- arquivos_parquet |> capesR::read_capes_data()

dados_completos

## Busca por trabalhos ----

resultado <- dplyr::bind_rows(capesR::search_capes_text(dados_completos,
                                                        term = "anuran diversity",
                                                        field = "titulo"),
                              capesR::search_capes_text(dados_completos,
                                                        term = "anuran diversity",
                                                        field = "resumo")) |>
  dplyr::distinct()

## Visualiza os resultados ----

resultado

resultado |> dplyr::glimpse()

resultado  |>
  dplyr::filter(dplyr::n() == 1,
                .by = autoria) |>
  dplyr::distinct(.keep_all = TRUE) |>
  dplyr::pull(titulo)
