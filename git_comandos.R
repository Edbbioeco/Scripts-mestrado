# Pacotes ----

library(gert)

# Adicionando arquivo ----

gert::git_add(list.files(pattern = "git_comandos.R")) |>
  as.data.frame()

# Commitando ----

gert::git_commit("Script os comandos de git")

# Pushando ----

gert::git_push(remote = "origin", force = TRUE)

# Pullando ----

gert::git_pull()

# Resetando ----

gert::git_reset_soft("HEAD~1")

gert::git_reset_hard("HEAD~1")
