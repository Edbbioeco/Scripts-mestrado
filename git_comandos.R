# Pacotes ----

library(gert)

# List de arquivos .R ----

gert::git_status() |>
  as.data.frame()

# Adicionando arquivo ----

gert::git_add(list.files(pattern = ".R$")) |>
  as.data.frame()

# Commitando ----

gert::git_commit("Scripts paralelos")

# Pushando ----

gert::git_push(remote = "origin", force = TRUE)

# Pullando ----

gert::git_pull()

# Resetando ----

gert::git_reset_soft("HEAD~1")

gert::git_reset_hard("HEAD~1")
