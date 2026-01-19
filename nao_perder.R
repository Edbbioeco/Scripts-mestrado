parcelas <- sf::st_read("saltinho_ppbio_parcelas.shp")

centroides <- parcelas |>
  dplyr::filter(Trlh.Pr != "1-1") |>
  sf::st_centroid()

centroides

borda_saltinho <- sf::st_read("borda_saltinho.kml") |>
  sf::st_cast("POLYGON") |>
  sf::st_as_sf() |>
  sf::st_set_crs(4674)

borda_saltinho |> plot()

borda <- borda_saltinho |>
  sf::st_boundary()

borda |> plot()

sf::st_distance(centroides, borda)

div_alfa2 <- div_alfa |>
  dplyr::mutate(distancia = sf::st_distance(centroides, borda) |> as.numeric()) |>
  dplyr::rename("Q0" = `0`,
                "Q1" = `1`)

div_alfa2

modelo_0 <- glm(Q0 ~ distancia,
    data = div_alfa2,
    family = poisson())

par(mfrow = c(2, 2)); modelo_0 |> plot(); par(mfrow = c(1, 1))

modelo_0 |>
  summary()

div_alfa2 |>
  ggplot(aes(distancia, Q0)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm")

modelo_1 <- lm(Q1 ~ distancia,
               data = div_alfa2)

modelo_1 |> performance::check_model(check = c("vif",
                                               "qq",
                                               "normality",
                                               "homogeneity"))
modelo_1 |> summary()

div_alfa2 |>
  ggplot(aes(distancia, Q1)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm")
