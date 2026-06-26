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
