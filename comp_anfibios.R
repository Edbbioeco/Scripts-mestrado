dados_sps <- readxl::read_xlsx("inventario_anfibios_caatinga.xlsx")

dados_sps %>% View()

dados_sps_trat <- dados_sps %>%
  dplyr::select(-Família) %>%
  dplyr::mutate(Incidência = dplyr::case_when(Incidência %>% is.na() ~ 1,
                                              .default = Incidência)) %>%
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Incidência,
                     values_fill = 0)

dados_sps_trat %>% view()

local <- dados_sps_trat$Local

dados_sps_trat <- dados_sps_trat %>%
  dplyr::select(-Local)

rownames(dados_sps_trat) <- local

dados_sps_trat

sor <- dados_sps_trat %>%
  betapart::beta.pair()

sor_matriz <- sor$beta.sor %>%
  as.matrix()

sor_matriz[upper.tri(sor_matriz)] <- NA

sor_matriz <- sor_matriz %>%
  reshape2::melt() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não")) %>%
  dplyr::filter(igual != "Sim") %>%
  dplyr::select(-igual) %>%
  dplyr::mutate(tipo = "total")

sor_matriz

subs_matriz <- sor$beta.sim %>%
  as.matrix()

subs_matriz[upper.tri(subs_matriz)] <- NA

subs_matriz <- subs_matriz %>%
  reshape2::melt() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não")) %>%
  dplyr::filter(igual != "Sim") %>%
  dplyr::select(-igual) %>%
  dplyr::mutate(tipo = "Substituição")

subs_matriz

ani_matriz <- sor$beta.sne %>%
  as.matrix()

ani_matriz[upper.tri(ani_matriz)] <- NA

ani_matriz <- ani_matriz %>%
  reshape2::melt() %>%
  tidyr::drop_na() %>%
  dplyr::mutate(igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                         .default = "Não")) %>%
  dplyr::filter(igual != "Sim") %>%
  dplyr::select(-igual) %>%
  dplyr::mutate(tipo = "Aninhamento")

ani_matriz

comp_df <- dplyr::bind_rows(sor_matriz, subs_matriz, ani_matriz) %>%
  dplyr::mutate(tipo = tipo %>% forcats::fct_relevel(c("total", "Substituição", "Aninhamento")))


comp_df

comp_df %>%
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black", linewidth = 0.5) +
  coord_equal() +
  facet_wrap(~tipo) +
  scale_fill_viridis_c(breaks = seq(0, 1, 0.2),
                       limits = c(0, 1)) +
  labs(fill = "Dissimilaridade",
       x = NULL,
       y =  NULL) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 20,
                               frame.colour = "black",
                               ticks.colour = "black",
                               ticks.linewidth = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.text.x = element_text(color = "black", size = 15, angle = 90, hjust = 1),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 17.5),
        legend.position = "bottom",
        panel.background = element_rect(fill = "transparent", color = "black"))

dados_sps_trat_dist <- dados_sps_trat %>%
  vegan::vegdist(method = "jaccard")

vegan::adonis2(dados_sps_trat_dist ~ UF, data = dados)

nmds <- dados_sps_trat %>%
  vegan::metaMDS(distance = "jaccard", k = 2)

nmds

nmds %>%
  fortify() %>%
  dplyr::filter(score == "sites") %>%
  dplyr::mutate(Estado = dados$UF) %>%
  ggplot(aes(NMDS1, NMDS2, fill = Estado, label = label)) +
  geom_label(vjust = "inward", hjust = "inward") +
  scale_x_continuous(breaks = seq(-0.8, 1.8, 0.5))

geo_dist <- dados_trat %>%
  as.data.frame() %>%
  dplyr::select(Longitude:Latitude) %>%
  fields::rdist.earth(miles = FALSE) %>%
  as.dist()

vegan::mantel(geo_dist, dados_sps_trat_dist)
