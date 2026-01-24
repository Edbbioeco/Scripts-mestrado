modelo_pristimantis <- glm(`Pristimantis ramagii` ~ `Distância dos corpos hídricos`,
                           data = df_ocupacao,
                           family = poisson(link = "log"))

### Modelo linear ----

modelo_pristimantis <- glm(`Pristimantis ramagii` ~ .,
                           data = df_ocupacao |>
                             dplyr::select(2, 5:8),
                           family = poisson(link = "log"))

### Pressupostos do modelo ----

modelo_pristimantis |>
  DHARMa::simulateResiduals(plot = TRUE)

### Estatísticas do modelo -----

modelo_pristimantis |> summary()

modelo_adenomera <- glm(`Adenomera hylaedactyla` ~ `Área das poças`,
                           data = df_ocupacao,
                           family = poisson(link = "log"))

modelo_adenomera |> summary()

modelo_rhinella <- glm(`Rhinella hoogmoedi` ~ `Distância dos corpos hídricos`,
                        data = df_ocupacao,
                        family = poisson(link = "log"))

modelo_rhinella |> summary()

# `Abertura de dossel`            -1.736e+01  6.282e+00  -2.764 0.005707 **
# `Altura da serrapilheira`       -2.086e-01  6.070e-02  -3.437 0.000588 ***
#  `Área das poças`                 5.337e-02  1.775e-02   3.008 0.002633 **
#  `Distância dos corpos hídricos`

rodar_modelos_pristimantis <- function(id){

  modelo <-  glm(`Pristimantis ramagii` ~ .,
                 data = df_ocupacao[, c(2, id)],
                 family = poisson(link = "log"))

  nome <- df_ocupacao[, id] |> names() |> stringr::word(1)

  assign(paste0("modelo_pristimantis_", nome),
         modelo,
         envir = globalenv())

}

purrr::walk(5:8, rodar_modelos_pristimantis)

lista <- ls(pattern = "modelo_pristimantis_") |>
  mget(envir = globalenv())

rodar_preditores_pristimantis <- function(lista){

  resultados <- lista |>
    summary() %>%
    .$coefficient |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::mutate(AIC = lista |> AIC()) |>
    dplyr::relocate(AIC,
                    .before = `z value`) |>
    dplyr::filter(!rowname |> stringr::str_detect("Intercept"))

  nome <- resultados$rownam |> stringr::word(1)

  assign(paste0("resultados_pristimantis_", nome),
         resultados,
         envir = globalenv())

}

purrr::walk(lista, rodar_preditores_pristimantis)

modelo_pristimantis_Abertura |>
  summary() %>%
  .$coefficient |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::mutate(AIC = modelo_pristimantis_Abertura |> AIC()) |>
  dplyr::relocate(AIC,
                  .before = `z value`) |>
  dplyr::filter(!rowname |> stringr::str_detect("Intercept"))
ls(pattern = "resultados_pristimantis_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()
