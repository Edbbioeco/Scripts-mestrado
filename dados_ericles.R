# Pacotes ----

library(tidyverse)

library(flextable)

library(readxl)

library(vegan)

library(pairwiseAdonis)

library(ggvegan)

# Dados ----

## Importando ----

dados_ericles <- readr::read_table("dfarea.txt")

comp_acaro <- readxl::read_xlsx("comp_acaros.xlsx")

## Visualizando ----

dados_ericles

comp_acaro

## Tratando ----

dados_ericles_trat <- dados_ericles %>%
  dplyr::mutate(local = dplyr::case_match(local,
                                          "mataatlantica" ~ "Mata Atlântica",
                                          "caatinga" ~ "Caatinga"),
                valor = valor %>% as.numeric()) %>%
  tidyr::drop_na()

dados_ericles_trat %>% View()

dados_ericles_trat %>%
  dplyr::summarise(n = n(), .by = c(local, grupo))

comp_acaro_trat <- comp_acaro %>%
  dplyr::select(c(abelha, morfo1:morfo13))

comp_acaro_trat %>% View()

comp_acaro_trat

comp_acaro_trat_2 <- comp_acaro_trat %>%
  dplyr::select_if(is.numeric)

df <- data.frame(logic = rowSums(comp_acaro_trat_2 != 0) > 0,
                 id = 1:452) %>%
  dplyr::filter(logic == TRUE) %>%
  dplyr::pull(id)

df

comp_acaro_trat_2 <- comp_acaro_trat_2[df, ] %>%
  dplyr::mutate(abelha = comp_acaro_trat[df, ]$abelha) %>%
  na.omit()

comp_acaro_trat_2[df, ] %>%
  dplyr::mutate(abelha = comp_acaro_trat[df, ]$abelha)

comp_acaro_trat_2[is.na(comp_acaro_trat_2)] <- 0

comp_acaro_trat_2 %>% view()

# Gráfico ----

## Criando ----

dados_ericles_trat %>%
  ggplot(aes(local, valor, fill = grupo)) +
  geom_jitter(shape = 21, size = 3, color = "black", width = 0.1) +
  facet_wrap(~ grupo) +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  labs(x = "Bioma",
       y = "Valor",
       fill = "Grupo") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom",
        strip.text = element_text(color = "black", size = 12))

## Salvando ----

ggsave("grafico_ericles.png", height = 10, width = 12)

# Modelando ----

## Criando o modelo ----

anova_ericles <- glm(valor ~ local * grupo,
                     data = dados_ericles_trat,
                     family = quasipoisson(link = "log"))

## Avaliando o modelo ----

anova_ericles %>% plot()

## Estatísticas ----

anova_ericles %>%
  car::Anova()

## Comparando os grupos ----

comparacoes <- emmeans::emmeans(anova_ericles, ~ local * grupo) %>%
  pairs(adjust = "tukey")

comparacoes_flex <- comparacoes %>%
  as.data.frame() %>%
  dplyr::select(c(1, 5:6)) %>%
  dplyr::mutate(Significant = dplyr::case_when(p.value < 0.05 ~ "Sim",
                                               .default = "Não"),
                p.value = p.value %>% round(4),
                z.ratio = z.ratio %>% round(4)) %>%
  dplyr::rename("Contraste" = contrast,
                "Z" = z.ratio,
                "p" = p.value,
                "Diferença significativa" = Significant) %>%
  flextable::flextable() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::width(width = 1.25)

comparacoes_flex

comparacoes_flex %>%
  flextable::save_as_docx(path = "./tabela_ericles.docx")

# Permanova ----

## Calculando a Permanova ----

### Criando a matriz ----

dist_bray <- comp_acaro_trat_2 %>%
  dplyr::select_if(is.numeric) %>%
  vegan::vegdist()

dist_bray

### Calculando ----

permanova <- vegan::adonis2(dist_bray ~ abelha, data = comp_acaro_trat_2)

permanova

### par-a-par ----

pair_flex <- pairwiseAdonis::pairwise.adonis(comp_acaro_trat_2 %>%
                                   dplyr::select_if(is.numeric),
                                 comp_acaro_trat_2$abelha) %>%
  dplyr::select(c(1:2, 4:6)) %>%
  dplyr::rename("Pares" = pairs,
                "GL" = Df,
                "F" = F.Model,
                "R²" = R2,
                "p" = p.value) %>%
  dplyr::mutate(`F` = `F` %>% round(4),
                `R²` = `R²` %>% round(4)) %>%
  flextable::flextable() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::width(width = 1)

pair_flex

pair_flex %>%
  flextable::save_as_docx(path = "./tabela_ericles_par_a_par_permanova.docx")

## NMDS ----

nmds <- comp_acaro_trat_2 %>%
  dplyr::select_if(is.numeric) %>%
  vegan::metaMDS(distance = "bray", k = 2)

nmds %>%
  ggplot2::fortify() %>%
  dplyr::filter(score == "sites") %>%
  dplyr::mutate(Abelha = comp_acaro_trat_2$abelha) %>%
  ggplot(aes(NMDS1, NMDS2, fill = Abelha)) +
  geom_point(shape = 21, color = "black", size = 5) +
  scale_fill_viridis_d(option = "turbo") +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "top",
        strip.text = element_text(color = "black", size = 12))

ggsave("nmds_ericles.png", height = 10, width = 12)

nmds_flex <- nmds %>%
  ggplot2::fortify() %>%
  dplyr::filter(score == "sites") %>%
  dplyr::mutate(Abelha = comp_acaro_trat_2$abelha) %>%
  dplyr::select(-2) %>%
  flextable::flextable() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::width(width = 1)

nmds_flex

nmds_flex %>%
  flextable::save_as_docx(path = "./tabela_ericles_nmds.docx")
