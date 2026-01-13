# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(sf)

library(sp)

library(writexl)

# Dados ----

## Coordenadas ----

### Importando ----

coords <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/parcelas_saltinho.xlsx",
                            sheet = 2)

### tratando ----

coords_trat <- coords %>%
  dplyr::filter(!Latitude %>% is.na) %>%
  dplyr::mutate(`Trilha-Parcela` = paste0(paste0("Trilha ", Trilha),
                                          "-",
                                          paste0("Parcela ", Parcela)),
                Latitude = Latitude %>% parzer::parse_lat(),
                Longitude = Longitude %>% parzer::parse_lon())

### Visualizando ----

coords_trat

## Novas parcelas ----

### Importando ----

lista_shp <- list.files(path = "./Saltinho_Larissa/Vetorial_WGS84",
                        pattern = ".shp")

lista_shp <- lista_shp[seq(1, 11, 2)]

lista_shp

importar_shp <- function(x, y){

  shps <- sf::st_read(paste0("./Saltinho_Larissa/Vetorial_WGS84/", x))

  assign(y,
         shps,
         envir = globalenv())

}

purrr::walk2(lista_shp,
             lista_shp %>% stringr::str_remove(".shp"),
             importar_shp)

### Visualizando ----

lista_shp %>%
  stringr::str_remove(".shp") %>%
  mget(envir = globalenv()) %>%
  dplyr::bind_rows() %>%
  ggplot() +
  geom_sf()

# Extraindo as coordenadas ----

## Nomes das parcelas ----

nome_parcelas <- c("Ripária 2",
                   paste0("1 ", 3:4),
                   paste0("2 ", 2:4))

nome_parcelas

## Transformar em dataframe ----

shp_p_df <- function(x, y){

  lista_de_shps <- lista_shp %>%
    stringr::str_remove(".shp")

  df <- x %>%
    tibble::as_tibble() %>%
    dplyr::select(3:6) %>%
    dplyr::mutate(Longitude = x %>%
                    sf::st_coordinates() %>%
                    as_tibble() %>%
                    dplyr::pull(X) %>%
                    sp::dd2dms(NS = FALSE) %>%
                    as.character() %>%
                    stringr::str_replace("d", "°"),
                  Latitude = x %>%
                    sf::st_coordinates() %>%
                    as_tibble() %>%
                    dplyr::pull(Y) %>%
                    sp::dd2dms(NS = TRUE) %>%
                    as.character() %>%
                    stringr::str_replace("d", "°"),
                  Trilha = nome_parcelas[y] %>% stringr::word(1),
                  Parcela = nome_parcelas[y] %>% stringr::word(2))

  assign(paste0("df_",
                lista_de_shps[y]),
         df,
         envir = globalenv())

}

purrr::walk2(lista_shp %>%
               stringr::str_remove(".shp") %>%
               mget(envir = globalenv()),
            1:length(nome_parcelas),
            shp_p_df)

## Unindo os dataframes ----

df_shp_unidos <- ls(pattern = "df_") %>%
  mget(envir = globalenv()) %>%
  bind_rows()

df_shp_unidos

# Exportando ----

df_shp_unidos %>%
  writexl::write_xlsx("parcelas_unidas.xlsx")
