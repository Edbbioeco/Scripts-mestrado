# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(sp)

library(crayon)

library(rgbif)

library(writexl)

# Dados ----

## Modelo Darwin Core ----

### Importar ----

darwin_core <- readxl::read_xlsx("Template_lista_especies.xlsx")

### Visualizar ----

darwin_core

darwin_core |> dplyr::glimpse()

## Dados do levantamento de anuros ----

### Importar ----

anuros <- readxl::read_xlsx("levantamento_anuros.xlsx")

### Visualizar ----

anuros

anuros |> dplyr::glimpse()

## Coordenada das parcelas ----

### Importar ----

coords <-sf::st_read("saltinho_ppbio_parcelas.shp")

### Visualizar ------

coords

ggplot() +
  geom_sf(data = coords)

# Modelo ----

## Criar o modelo base ----

modelo <- darwin_core |>
  dplyr::mutate(baseOfRecord = "HumanObservation",
                occurrenceID = "",
                recordedBy = "",
                decimalLongitude = "",
                decimalLatitude = "",
                country = "") |>
  dplyr::relocate(recordedBy, .after = license) |>
  dplyr::relocate(decimalLongitude:country, .before = locality) |>
  dplyr::relocate(baseOfRecord:occurrenceID, .before = datasetName)

modelo

modelo |> dplyr::glimpse()

## Coordenadas das parcelas ----

coords_gms <- coords |>
  sf::st_centroid() |>
  sf::st_coordinates() |>
  as.data.frame() |>
  dplyr::rename("decimalLongitude" = X,
                "decimalLatitude" = Y) |>
  dplyr::mutate(decimalLongitude = decimalLongitude |>
                  sp::dd2dms(NS = FALSE) |>
                  as.character() |>
                  stringr::str_replace("d", "°"),
                decimalLatitude = decimalLatitude |>
                  sp::dd2dms(NS = TRUE) |>
                  as.character() |>
                  stringr::str_replace("d", "°"),
                `Unidade Amostral` = c(paste0("T1P", 1:4),
                                       paste0("T2P", 1:4),
                                       paste0("T3P", 1:2),
                                       paste0("R", 1:2)))

coords_gms

## Conferindo se os taxons podem ser rastreados até seus IDs ----

taxon <- anuros |>
  dplyr::filter(!Espécie |> is.na()) |>
  dplyr::pull(Espécie) |>
  unique()

taxon

taxonid <- c()

testar_id <- function(taxon){

  paste0("Testando para: ", taxon) |>
    crayon::green() |>
    message()

  idtaxon <- taxon |>
    rgbif::name_backbone_checklist(sleep = 1, bucket_size = 10) |>
    dplyr::pull(1)

  taxonid <<- c(taxonid, idtaxon)

}

purrr::map(taxon, testar_id)

taxonid

df_taxonid <- tibble::tibble(Espécie = taxon,
                             taxonID = taxonid,
                             autoria = c(
                               "Pristimantis ramagii"        = "(Boulenger, 1888)",
                               "Adenomera hylaedactyla"      = "(Cope, 1868)",
                               "Rhinella hoogmoedi"          = "Caramaschi and Pombal, 2006",
                               "Rhinella granulosa"          = "(Spix, 1824)",
                               "Frostius pernambucensis"     = "(Bokermann, 1962)",
                               "Leptodactylus natalensis"    = "Lutz, 1930",
                               "Dendropsophus minutus"       = "(Peters, 1872)",
                               "Leptodactylus troglodytes"   = "Lutz, 1926",
                               "Dendropsophus elegans"       = "(Wied-Neuwied, 1824)",
                               "Dendropsophus haddadi"       = "(Bastos and Pombal, 1996)",
                               "Pithecopus gonzagai"         = "Andrade, Haga, Ferreira, Recco-Pimentel, Toledo and Bruschi, 2020",
                               "Dryadobates alagoanus"       = "(Bokermann, 1967)",
                               "Dendropsophus oliveirai"     = "(Bokermann, 1963)",
                               "Adelophryne nordestina"      = "Lourenco-de-Moraes et al., 2021",
                               "Coleodactylus meridionalis"  = "(Boulenger, 1888)",
                               "Dendropsophus branneri"      = "(Cochran, 1948)",
                               "Physalaemus cuvieri"         = "Fitzinger, 1826",
                               "Boana atlantica"             = "(Caramaschi and Velosa, 1996)",
                               "Leptodactylus natalensis"    = "Lutz, 1930",
                               "Elachistocleis cesari"       = "(Miranda-Ribeiro, 1920)",
                               "Enyalius catenatus"          = "(Wied-Neuwied, 1821)",
                               "Rhinella crucifer"           = "(Wied-Neuwied, 1821)"),
                             nomes_vernaculares = c(
                               "Pristimantis ramagii"       = "Paraiba Robber Frog",
                               "Adenomera hylaedactyla"     = "Dark-spotted Thin-toed Frog",
                               "Rhinella hoogmoedi"         = NA,
                               "Rhinella granulosa"         = "Granular Toad",
                               "Frostius pernambucensis"    = "Frost's Toad",
                               "Leptodactylus natalensis"   = NA,
                               "Dendropsophus minutus"      = "Lesser Treefrog",
                               "Leptodactylus troglodytes"  = "Pernambuco White-lipped Frog",
                               "Dendropsophus elegans"      = "Elegant Forest Treefrog",
                               "Dendropsophus haddadi"      = NA,
                               "Pithecopus gonzagai"        = NA,
                               "Dryadobates alagoanus"      = NA,
                               "Dendropsophus oliveirai"    = "Xeric Treefrog",
                               "Adelophryne nordestina"     = NA,
                               "Coleodactylus meridionalis" = "Meridian Gecko",
                               "Dendropsophus branneri"     = NA,
                               "Physalaemus cuvieri"        = "Barking Frog",
                               "Boana atlantica"            = NA,
                               "Leptodactylus natalensis"   = NA,
                               "Elachistocleis cesari"      = NA,
                               "Enyalius catenatus"         = "Wied's Fathead Anole",
                               "Rhinella crucifer"          = "Striped Toad"),
                             iucn_status = c(
                               "Pristimantis ramagii"       = "Least Concern",
                               "Adenomera hylaedactyla"     = "Least Concern",
                               "Rhinella hoogmoedi"         = "Least Concern",
                               "Rhinella granulosa"         = "Least Concern",
                               "Frostius pernambucensis"    = "Least Concern",
                               "Leptodactylus natalensis"   = "Least Concern",
                               "Dendropsophus minutus"      = "Least Concern",
                               "Leptodactylus troglodytes"  = "Least Concern",
                               "Dendropsophus elegans"      = "Least Concern",
                               "Dendropsophus haddadi"      = "Least Concern",
                               "Pithecopus gonzagai"        = "Least Concern",
                               "Dryadobates alagoanus"      = "Least Concern",
                               "Dendropsophus oliveirai"    = "Least Concern",
                               "Adelophryne nordestina"     = NA,
                               "Coleodactylus meridionalis" = "Least Concern",
                               "Dendropsophus branneri"     = "Least Concern",
                               "Physalaemus cuvieri"        = "Least Concern",
                               "Boana atlantica"            = "Least Concern",
                               "Leptodactylus natalensis"   = "Least Concern",
                               "Elachistocleis cesari"      = "Least Concern",
                               "Enyalius catenatus"         = "Least Concern",
                               "Rhinella crucifer"          = "Least Concern"))

df_taxonid

## Data frame das informações a serem copiadas ----

df_copy <- anuros |>
  dplyr::select(`Unidade Amostral`, Ordem:Espécie, Data) |>
  dplyr::left_join(coords_gms,
                   by = "Unidade Amostral") |>
  dplyr::filter(!Espécie |> is.na()) |>
  dplyr::mutate(Data = Data |> lubridate::ymd()) |>
  dplyr::left_join(df_taxonid,
                   by = "Espécie") |>
  as.data.frame()

df_copy

## Completando no modelo -----

modelo <- modelo[rep(1, nrow(df_copy)), ]

modelo |>
  dplyr::mutate(taxonID = df_copy$taxonID,
                scientificName = df_copy$Espécie,
                taxonRank = "Species",
                scientificNameAuthorship = paste0(df_copy$Espécie,
                                                  " ",
                                                  df_copy$autoria),
                kingdom = "Animalia",
                phylum = "Chordata",
                class = dplyr::case_when(scientificName |>
                                           stringr::str_detect("Enyaluius|Coleodactyus") ~ "Reptilia",
                                         .default = "Amphibia"),
                order = df_copy$Ordem,
                family = df_copy$Família,
                genus = df_copy$Gênero,
                specificEpithet = df_copy$Epípeto,
                infraspecificEpithet = NA) |>
  tidyr::fill(dplyr::everything()) |>
  as.data.frame()

