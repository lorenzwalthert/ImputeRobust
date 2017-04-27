#' extract degrees of freedom from a gam object
#' @param fit an object returned by gam()
dfs_from_gam <- function(fit) {
  nme <- attr(terms(fit$formula), "term.labels")
  edf_list <- fit$edf
  vapply(nme, sum_edf, numeric(1), edf_list)
}

#' sum all estimated degrees of freedom that belong to same coefficient
#' @param coef a coefficient name from a gam
#' @param edf_list a list with all estimated degrees of freedom
#' @example dfs_from_gam(fit)
sum_edf <- function(coef, edf_list) {
  sum(edf_list[grep(coef, names(edf_list), fixed = TRUE)])
}


#' turn a degrees of freedom list into a term
#' @example  dfs_to_term(dfs_from_gam(fit))
dfs_to_term <- function(df_list) {
  vapply(seq_len(length(df_list)), function(x) {
    current <- df_list[x]
    nme <- names(current)
    current <- gsub("s(", "cs(", nme, fixed = TRUE)
    gsub(")", paste0(", df = ", round(df_list[x], 5), ")"), current, fixed = TRUE)
  }, character(1))
}


#' collapse terms into a formula
#' @examples 
#' elements <- dfs_to_term(dfs_from_gam(fit))
#' term_to_str_formula(elements, "pollen")
term_to_str_formula <- function(elements, response) {
  paste(response, "~", paste(elements, collapse = " + "))
}


#' extract a formula from a gam object 
#' extract estimated degrees of freedom from a gam object and return a formula
#'   that can serve as an input for a gamlss estimation
#' @example extract_formula(fit)
extract_formula <- function(fit) {
  fit %>%
    dfs_from_gam() %>%
    dfs_to_term() %>%
    term_to_str_formula("pollen")
}
