# Pacotes ----

library(usethis)

# Inicial o git ----

usethis::use_git()

# Configure o usuario e email ----

usethis::use_git_config(user.name = "Edbbioeco",
                        user.email = "edsonbbiologia@gmail.com")

# Settando o repositório ----

usethis::proj_get()

## Repositório privado ----

usethis::use_git_remote(name = "origin",
                        url = "https://github.com/Edbbioeco/scripts_tabelas_dados_mestrado.git",
                        overwrite = TRUE)

## Repositório púbico ----

usethis::use_git_remote(name = "backup",
                        url = "https://github.com/Edbbioeco/Scripts-mestrado.git",
                        overwrite = TRUE)

# Renomear o branch do master para main ----

usethis::git_default_branch_configure()

usethis::git_default_branch_rename(from = "master", to = "main")
