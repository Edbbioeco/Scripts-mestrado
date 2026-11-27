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

## Ruggiero para abundância ----

beta_rlb_ab <- function(comm){

  comm <- as.matrix(comm)

  sitios <- rownames(comm)

  n <- nrow(comm)

  tidyr::expand_grid(
    j = seq_len(n),
    k = seq_len(n)) |>
    dplyr::mutate(
      beta = purrr::map2_dbl(
        .x = j,
        .y = k,
        .f = ~ sum(comm[.x, ] - pmin(comm[.x, ], comm[.y, ])) / sum(comm[.x, ])
      ),
      beta = dplyr::if_else(j == k, NA_real_, beta)) |>
    dplyr::pull(beta) |>
    matrix(
      nrow = n,
      ncol = n,
      byrow = TRUE,
      dimnames = list(sitios, sitios)
    )

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
