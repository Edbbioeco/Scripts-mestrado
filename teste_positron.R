# Pacotes ----

library(ecodados)

library(betapart)

library(vegan)

library(reshape2)

library(tidyverse)

library(sjPlot)

# Dados ----

## Importando ----

dados <- ecodados::composicao_anuros_div_taxonomica

prec <- ecodados::precipitacao_div_taxonomica

## Visualizando ----

dados

prec

# Diversidade ----

## Calculando a diversidade ----

beta_div <- dados %>%
  betapart::beta.pair.abund()

beta_div

## Criando os vetors ----

dis_com_gra <- beta_div %>%
  .$beta.bray.gra %>%
  reshape2::melt() %>%
  dplyr::pull(value)

dis_com_gra

dis_com_bal <- beta_div %>%
  .$beta.bray.bal %>%
  reshape2::melt() %>%
  dplyr::pull(value)

dis_com_bal

## Calculando a dissimilaridade ambiental

dis_amb <- prec %>%
  vegan::vegdist(method = "euclidean") %>%
  reshape2::melt() %>%
  dplyr::pull(value)

dis_amb

# Modelo ----

## Criando um tibble ----

df <- tibble::tibble(`Dissimilaridade de Gradiente de Variação` = dis_com_gra,
                     `Dissimilaridade de Variação Balanceada` = dis_com_bal,
                     `Dissimilaridade Ambiental` = dis_amb)

df

## criando os modelos ----

modelo_gra <- lm(`Dissimilaridade de Gradiente de Variação` ~ `Dissimilaridade Ambiental`, data = df)

modelo_bal <- lm(`Dissimilaridade de Variação Balanceada` ~ `Dissimilaridade Ambiental`, data = df)

## Avaliando os modelos ----

modelo_gra %>%
  sjPlot::plot_model(type = "diag") %>%
  sjPlot::plot_grid()

modelo_bal %>%
  sjPlot::plot_model(type = "diag") %>%
  sjPlot::plot_grid()

## Estatísticas dos modelos ----

modelo_gra %>%
  summary()

modelo_bal %>%
  summary()

stats::qf(p = 0.05, df1 = 1, df2 = 43, lower.tail = FALSE)

stats::qt(p = 0.05, df = 43, lower.tail = FALSE)

## Gráfico ----

### Criando um novo tibble ----

df_gra <- tibble::tibble(`Dissimilaridade de Composição` = dis_com_gra,
                         `Dissimilaridade Ambiental` = dis_amb,
                         Tipo = "Gradiente de Variação")

df_gra

df_bal <- tibble::tibble(`Dissimilaridade de Composição` = dis_com_bal,
                         `Dissimilaridade Ambiental` = dis_amb,
                         Tipo = "Variação Balancea")

df_bal

df_unido <- dplyr::bind_rows(df_gra, df_bal)

df_unido

### Gráfico ----

df_unido %>%
  ggplot(aes(`Dissimilaridade Ambiental`, `Dissimilaridade de Composição`,
             fill = Tipo,
             color = Tipo))+
  geom_point(shape = 21, color = "black", size = 2.5) +
  facet_wrap(~ Tipo) +
  geom_smooth(method = "lm") +
  scale_fill_manual(values = c("gold", "lightblue")) +
  scale_color_manual(values = c("gold4", "lightblue4")) +
  theme_bw() +
  theme(legend.position = "none")


modelo_bal %>% report::report()
