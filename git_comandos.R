# Pacotes ----

library(gert)

# List de arquivos .R ----

gert::git_status() |>
  as.data.frame() |>
  dplyr::filter(file |> stringr::str_detect(".R$"))

# Adicionando arquivo ----

gert::git_add(list.files(pattern = "^git_")) |>
  as.data.frame()

# Commitando ----

gert::git_commit("Script para os comandos de Git")

# Pushando ----

gert::git_push(remote = "mestrado", force = TRUE)

# Pullando ----

gert::git_pull(remote = "mestrado")

# Resetando ----

gert::git_reset_mixed() |> as.data.frame()

gert::git_reset_soft("HEAD~1") |> as.data.frame()

# Removendo arquivos do repositório ----

gert::git_rm(list.files(pattern = ".xlsx$|.csv$|.shp$|.shx$|.dbf$|.prj$|.docx$|.png$|.kmz$|.cpg$|.tif$|.bib|.zip$")) |>
  as.data.frame()

gert::git_commit("Removendo arquivos não .R do repositório")

#gert::git_push(remote = "privado", force = TRUE)

gert::git_push(remote = "mestrado", force = TRUE)

gert::git_pull(remote = "mestrado")
