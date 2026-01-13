# Pacotes ----

library(gert)

# List de arquivos .R ----

list.files(pattern = "div")

# Adicionando arquivo ----

gert::git_add(list.files(pattern = "div.*\\.R$")) |>
  as.data.frame()

# Commitando ----

gert::git_commit("Script para a criação do gráfico de vocalização de Dryadobates alagoanus")

# Pushando ----

gert::git_push(remote = "origin", force = TRUE)

# Pullando ----

gert::git_pull()

# Resetando ----

gert::git_reset_soft("HEAD~1")

gert::git_reset_hard("HEAD~1")
