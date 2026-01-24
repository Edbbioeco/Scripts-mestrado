# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

# Dados ----

## Importando ----

coords <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/parcelas_saltinho.xlsx")

## tratando ----

coords_trat <- coords %>%
  dplyr::filter(!Latitude %>% is.na) %>%
  dplyr::mutate(`Trilha-Parcela` = paste0(paste0("Trilha ", Trilha),
                                          "-",
                                          paste0("Parcela ", Parcela)),
                Latitude = Latitude %>% parzer::parse_lat(),
                Longitude = Longitude %>% parzer::parse_lon())

## Visualizando ----

coords_trat

# Função ----

## Criando a função ----

azimute_coord <- function(dados, latitude, longitude, azimute, distancia, continuo = TRUE, n){

  if(continuo == TRUE){

    resultados <- dados %>% as.data.frame()

    for(i in n:nrow(dados)) {

      coordenadas <- geosphere::destPoint(p = c(resultados[[latitude]][i-1],
                                                resultados[[longitude]][i-1]),
                                          b = resultados[[azimute]][i],
                                          d = distancia)

      resultados[[latitude]][i] <- coordenadas[1]

      resultados[[longitude]][i] <- coordenadas[2]

    }

    print(resultados)

  } else {

    resultados <- dados %>% as.data.frame()

    for(i in n) {

      coordenadas <- geosphere::destPoint(p = c(resultados[[latitude]][i-1],
                                                resultados[[longitude]][i-1]),
                                          b = resultados[[azimute]][i],
                                          d = distancia)

      resultados[[latitude]][i] <- coordenadas[1]

      resultados[[longitude]][i] <- coordenadas[2]

    }

    print(resultados)

  }

}

## Testando a função ----

### Criuando um data frame fictício ----

coords_trat2 <- coords_trat %>%
  dplyr::filter(Trilha == 1 & Parcela == 1) %>%
  dplyr::select(`Azimute (graus)`, Latitude:Longitude) %>%
  dplyr::mutate(n = 1:nrow(coords_trat %>%
                             dplyr::filter(Trilha == 1 & Parcela == 1)),
                Latitude = dplyr::case_when(n > 1 ~ NA,
                                            .default = Latitude),
                Longitude = dplyr::case_when(n > 1 ~ NA,
                                            .default = Longitude)) %>%
  dplyr::select(-n)

coords_trat2

### Testando ----

azimute_coord(dados = coords_trat2,
              latitude = "Latitude",
              longitude = "Longitude",
              azimute = "Azimute (graus)",
              distancia = 10,
              n = 2)

# Calculando as difereças de distancias ----

## Unindo os dataframes ----

coords_unidos <- azimute_coord(dados = coords_trat2,
              latitude = "Latitude",
              longitude = "Longitude",
              azimute = "Azimute (graus)",
              distancia = 10,
              n = 2) %>%
  dplyr::bind_cols(coords_trat %>%
                     dplyr::filter(Trilha == 1 & Parcela == 1) %>%
                     dplyr::select(Latitude:Longitude) %>%
                     dplyr::rename("lat" = Latitude,
                                   "lon" = Longitude) %>%
                     dplyr::mutate(n = 1:nrow(coords_trat %>%
                                                dplyr::filter(Trilha == 1 & Parcela == 1))))

coords_unidos

## Calculando a distância ----

distancias_coord <- coords_unidos %>%
  dplyr::mutate(distancia = geosphere::distGeo(p1 = coords_unidos[, c("lat", "lon")],
                                               p2 = coords_unidos[, c("Latitude", "Longitude")]
                                               )
                ) %>%
  dplyr::arrange(distancia %>% dplyr::desc())

distancias_coord

## Correlação entre as coordenadas ----

### Latitude ----

#### Pressupostso ----

distancias_coord$Latitude %>% shapiro.test()

distancias_coord %>%
  ggplot(aes(Latitude)) +
  geom_histogram()

distancias_coord$lat %>% shapiro.test()

distancias_coord %>%
  ggplot(aes(lat)) +
  geom_histogram()

#### Correlação ----

cor.test(distancias_coord$Latitude, distancias_coord$lat, method = "spearman")

distancias_coord %>%
  dplyr::arrange(n) %>%
  dplyr::mutate(Segmento = coords_trat %>%
                  dplyr::filter(Trilha == 1 & Parcela == 1) %>%
                  dplyr::pull(Segmento),
                Segmento = Segmento %>% forcats::fct_relevel(coords_trat %>%
                                                               dplyr::filter(Trilha == 1 & Parcela == 1) %>%
                                                               dplyr::pull(Segmento))) %>%
  tidyr::pivot_longer(cols = c(Latitude, lat),
                      names_to = "Tipo de latitude",
                      values_to = "Latitude") %>%
  dplyr::mutate(`Tipo de latitude` = dplyr::case_when(`Tipo de latitude` == "lat" ~ "Original",
                                                      `Tipo de latitude` == "Latitude" ~ "Função")) %>%
  ggplot(aes(Segmento, Latitude, fill = `Tipo de latitude`)) +
  geom_point(shape = 21, color = "black", size = 5) +
  scale_fill_manual(values = c("gold", "cyan4")) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))+
  theme_bw() +
  theme(axis.title = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        axis.text = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15, angle = 45, hjust = 1),
        legend.text = element_text(color = "black", size = 15),
        legend.position = "top")

### Longititude ----

#### Pressupostoo ----

distancias_coord$Longitude %>% shapiro.test()

distancias_coord %>%
  ggplot(aes(Longitude)) +
  geom_histogram()

distancias_coord$lon %>% shapiro.test()

distancias_coord %>%
  ggplot(aes(lon)) +
  geom_histogram()

#### Correlação ----

cor.test(distancias_coord$Longitude, distancias_coord$lon, method = "spearman")

distancias_coord %>%
  dplyr::arrange(n) %>%
  dplyr::mutate(Segmento = coords_trat %>%
                  dplyr::filter(Trilha == 1 & Parcela == 1) %>%
                  dplyr::pull(Segmento),
                Segmento = Segmento %>% forcats::fct_relevel(coords_trat %>%
                                                               dplyr::filter(Trilha == 1 & Parcela == 1) %>%
                                                               dplyr::pull(Segmento))) %>%
  tidyr::pivot_longer(cols = c(Longitude, lon),
                      names_to = "Tipo de longitude",
                      values_to = "Longitude") %>%
  dplyr::mutate(`Tipo de longitude` = dplyr::case_when(`Tipo de longitude` == "lon" ~ "Original",
                                                      `Tipo de longitude` == "Longitude" ~ "Função")) %>%
  ggplot(aes(Segmento, Longitude, fill = `Tipo de longitude`)) +
  geom_point(shape = 21, color = "black", size = 5) +
  scale_fill_manual(values = c("gold", "cyan4")) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))+
  theme_bw() +
  theme(axis.title = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        axis.text = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15, angle = 45, hjust = 1),
        legend.text = element_text(color = "black", size = 15),
        legend.position = "top")

## Distâncias e os segmentos ----

### Presupostos ----

distancias_coord$n %>% shapiro.test()

distancias_coord %>%
  ggplot(aes(n)) +
  geom_histogram()

distancias_coord$distancia %>% shapiro.test()

distancias_coord %>%
  ggplot(aes(distancia)) +
  geom_histogram()

### Correlação ----

cor.test(distancias_coord$n, distancias_coord$distancia)

distancias_coord %>%
  dplyr::arrange(n) %>%
  dplyr::mutate(Segmento = coords_trat %>%
                  dplyr::filter(Trilha == 1 & Parcela == 1) %>%
                  dplyr::pull(Segmento),
                Segmento = Segmento %>% forcats::fct_relevel(coords_trat %>%
                                                               dplyr::filter(Trilha == 1 & Parcela == 1) %>%
                                                               dplyr::pull(Segmento))) %>%
  ggplot(aes(Segmento, distancia)) +
  geom_point(shape = 21, color = "black", size = 5, fill = "gold") +
  theme_bw() +
  theme(axis.title = element_text(color = "black", size = 15),
        axis.text = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15, angle = 45, hjust = 1))
