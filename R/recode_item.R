#' recode_chr
#'
#' @param df data frame
#' @param col unquoted col
#' @param old_names character vector or regular expression
#' @param new_name atomic chr string
#' @param regex Logical, default F. If using a regular expression, set to T
#'
#' @return df
#' @export
recode_chr <- function(df, col, old_names, new_name, regex = F){

  if(regex){

  df %>%
    dplyr::pull({{col}}) %>%
    stringr::str_subset(old_names) %>%
    unique -> old_names}

  df %>%
    set_chr({{col}}) %>%
    dplyr::mutate({{col}} := ifelse({{col}} %in% old_names, new_name, {{col}}))
}

