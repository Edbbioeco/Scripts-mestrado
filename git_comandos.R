# Pacotes ----

library(gert)

# List de arquivos .R ----

gert::git_status() |>
  as.data.frame() |>
  dplyr::filter(file |> stringr::str_detect(".tif$"))

# Adicionando arquivo ----

gert::git_add(list.files(pattern = "git_comandos")) |>
  as.data.frame()

# Commitando ----

gert::git_commit("Scrip .R")

# Pushando ----

## Repositório privado ----

gert::git_push(remote = "privado", force = TRUE)

## Repositório público ----

#gert::git_push(remote = "publico", force = TRUE)

# Pullando ----

gert::git_pull()

# Resetando ----

gert::git_reset_mixed() |> as.data.frame()

gert::git_reset_soft("HEAD~1") |> as.data.frame()

gert::git_reset_hard("HEAD~1")

# Removendo arquivos do repositório ----

gert::git_rm(list.files(pattern = ".xlsx$|.csv$|.shp$|.shx$|.dbf$|.prj$|.docx$|.png$|.kmz$|.cpg$|.tif$|.bib|.zip$")) |>
  as.data.frame()

gert::git_commit("Removendo arquivos não .R do repositório")

gert::git_push(remote = "origin", force = TRUE)

gert::git_pull()
