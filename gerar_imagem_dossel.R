# Pacotes ----

library(coiR)

# Dados ----

img <- coiR::data_coir()

# Imagens ----

## Imagem normal ----

img[[4]] |>
  coiR::coir_crop()

ggsave(filename = "./apresentação/imagem_dossel.png",
       height = 12,
       width = 12)

## Imagem binária ----

img[[4]] |>
  coiR::coir_crop() |>
  coiR::coir_binarize()

ggsave(filename = "./apresentação/imagem_dossel_binario.png",
       height = 12,
       width = 12)
