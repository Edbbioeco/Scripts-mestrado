# List of packages ----

library(tidyverse)

library(betapart)

library(adespatial)

library(vegan)

library(terra)

library(classInt)

library(phyloregion)

library(VGAM)

library(ggplot2)

library(ggbeeswarm)

library(progress)

library(vegan)

# Create functions ----

## Ruggiero's dissimilarity for focal communities ----

beta.rug <- function(x){

  if(!is.matrix(x)){

    x <- as.matrix(x)

    }

  if(!is.numeric(x)){

    stop("The data in x is not numeric.", call. = TRUE)

    }

  xvals <- unique(as.vector(x))

  if(any(!is.element(xvals, c(0, 1)))){

    stop("The table contains values other than 0 and 1: data should be presence/absence.",
         call. = TRUE)}

  shared <- x %*% t(x)

  not.shared <- abs(sweep(shared, 1, diag(shared)))

  res <- not.shared / (shared + not.shared)

  return(res)

  }

## Uniqueness as average dissimilarity ----

uniqueness <- function(x, method = "arithmetic"){

  if(!is.matrix(x)){

    x <- as.matrix(x)

    }

  diag(x) <- NA

  method <- match.arg(method, c("arithmetic", "geometric"))

  switch(method,
         arithmetic = {
    res <- rowMeans(x, na.rm = TRUE)

    },
    geometric = {
    res <- exp(rowMeans(log(x + 0.01), na.rm = TRUE))

    })

  names(res) <- rownames(x)

  res

  }

# Effect of the dissimilarity index on uniqueness measures ----

## Scenarios ----

comm1 <- tibble::tibble(A1 = c(1,1,1,1,1,0,0,0,0,0),
                        A2 = c(0,0,0,0,0,1,1,1,1,0),
                        A3 = c(0,0,0,0,0,0,0,0,1,1),
                        A4 = c(1,0,0,0,0,0,0,0,0,0)) |>
  t() |>
  as.data.frame()

comm1

comm2 <- tibble::tibble(B1 = c(1,1,1,1,1,1,1,1,1,1),
                        B2 = c(1,1,1,1,1,1,0,0,0,0),
                        B3 = c(1,1,0,0,0,0,1,1,0,0),
                        B4 = c(1,1,0,0,0,0,0,0,1,0)) |>
  t() |>
  as.data.frame()

comm2

## Compute LCBD ----

### Sorensen ----

sor.LCBD <- purrr::map(
  ls(pattern = "comm") |>
    mget(envir = globalenv()),
  \(comunidade){

     adespatial::beta.div(comunidade, method = "sorensen")$LCBD

    },
  .progress = TRUE)

sor.LCBD

### Hellinger ----

hell.LCBD <- purrr::map(
  ls(pattern = "comm") |>
    mget(envir = globalenv()),
  \(comunidade){

    adespatial::beta.div(comunidade, method = "hellinger")$LCBD

  },
  .progress = TRUE)

hell.LCBD

## Compute U ----

### Sorensen ----

sor.dis <- purrr::map(
  ls(pattern = "comm") |>
    mget(envir = globalenv()),
  \(comunidade){

    betapart::beta.pair(comunidade)$beta.sor |>
      uniqueness()

  },
  .progress = TRUE)

sor.dis

### Sorensen ----

sim.dis <- purrr::map(
  ls(pattern = "comm") |>
    mget(envir = globalenv()),
  \(comunidade){

    betapart::beta.pair(comunidade)$beta.sim |>
      uniqueness()

  },
  .progress = TRUE)

sim.dis

### Ruggiero ----

rug.dis <- purrr::map(
  ls(pattern = "comm") |>
    mget(envir = globalenv()),
  \(comunidade){

    beta.rug(comunidade) |>
      uniqueness()

  },
  .progress = TRUE)

rug.dis

# Calcular endemismo pondrado ----

endem <- purrr::map(
  ls(pattern = "comm") |>
    mget(envir = globalenv()),
  \(comunidade){

    phyloregion::weighted_endemism(comunidade |>
                                     as.matrix() |>
                                     phyloregion::dense2sparse())

  },
  .progress = TRUE)

endem

# Visualization ----

dis_df <- purrr::map2_dfr(
  ls(pattern = ".dis$") |>
    c("endem") |>
    mget(envir = globalenv()),
  c("Ruggiero",
    "Simpson",
    "Sorensen",
    "Weightd endemism"),
  \(dis, name){

    dis |>
      dplyr::bind_cols() |>
      dplyr::mutate(Index = name,
                    Local = 1:dplyr::n())

    },
  .progress = TRUE)

dis_df
