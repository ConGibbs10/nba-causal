# return the most extreme element from a vector (i.e., most different from 0)
max_abs <- function(x) {
  x[which.max(abs(x))]
}

# return the position of the position of the most extreme element from a vector
# (i.e., most different from 0).
which_max_abs <- function(y) {
  x <- abs(y)
  z <- which(x == max(x))
  return(z[length(z)])
}

# the complement operator of %in%
'%!in%' <- function(x, y) {
  !('%in%'(x, y))
}

# count number of NAs in each column
na_count <-  function(x) {
  colSums(is.na(x))
}

# coalesce n vectors
coalesce2 <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x
  },
  list(...))
}

# get number of decimal places
ndecimal_places <- function(x) {
  require(tidyverse)
  if (x %% 10 != x | x >= 0.001) {
    x <- 0.00001
    warning("Choice of plotting_delta is unacceptable. Choosing plotting_delta of 0.00001.")
  }
  x <- as.character(x)
  return(as.numeric(stringr::str_sub(x, start = -1)))
}

# coalesce the rows within groups
coalesce_rows <- function(df) {
  return(dplyr::coalesce(!!!as.list(df)))
}

sample_n_groups = function(grouped_df,
                           size,
                           replace = FALSE,
                           weight = NULL) {
  grp_var <- grouped_df %>%
    dplyr::groups() %>%
    unlist() %>%
    as.character()
  random_grp <- grouped_df %>%
    dplyr::summarise() %>%
    dplyr::sample_n(., size, replace, weight) %>%
    dplyr::mutate(., unique_id = 1:NROW(.))
  res <- grouped_df %>%
    dplyr::right_join(random_grp, by = grp_var) %>%
    dplyr::group_by_(., grp_var) %>%
    dplyr::ungroup() %>%
    dplyr::select(.,-unique_id)
}

svd_influence <- function(mat, names = TRUE) {
  svd_out <- svd(mat)
  row_vars <- order(svd_out$u[, 1], decreasing = FALSE)
  col_vars <- order(svd_out$v[, 1], decreasing = FALSE)
  
  if (names) {
    row_vars <- rownames(mat)[row_vars]
    col_vars <- colnames(mat)[col_vars]
  }
  return(list(row = row_vars, col = col_vars))
}
