# Pacotes ----

library(usethis)

# Inicial o git ----

usethis::use_git()

# Configure o usuario e email ----

usethis::use_git_config(user.name = "Edbbioeco",
                        user.email = "edsonbbiologia@gmail.com")

# Settando o repositório ----

usethis::proj_get()

## Repositório púbico ----

usethis::use_git_remote(name = "publico",
                        url = "https://github.com/Edbbioeco/Scripts-mestrado.git",
                        overwrite = TRUE)

## Repositório privado ----

usethis::use_git_remote(name = "privado",
                        url = "https://github.com/Edbbioeco/scripts_tabelas_dados_mestrado.git",
                        overwrite = TRUE)

## Checar quais os repositórios settados ----

usethis::git_remotes()

# Renomear o branch do master para main ----

usethis::git_default_branch_configure()

usethis::git_default_branch_rename(from = "master", to = "main")
