# Pacotes ----

library(coiR)

# Dados ----

img <- coiR::data_coir()

# Imagem de dossel ----

## Imagem normal ----

img[[4]] |>
  coiR::coir_crop()

ggsave(filename = "./apresentação/imagem_dossel.png",
       height = 12,
       width = 12)

## Imagem binarizada ----

img[[4]] |>
  coiR::coir_crop(plot = FALSE) |>
  coiR::coir_binarize()

ggsave(filename = "./apresentação/imagem_dossel_binario.png",
       height = 12,
       width = 12)
