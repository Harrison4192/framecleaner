#' recode_chr
#'
#' @param df data frame
#' @param col unquoted col
#' @param old_names character vector or regular expression
#' @param new_name atomic chr string
#' @param regex Logical, default F. If using a regular expression, set to T
#' @param negate logicalm defailt F. If negating the regex, set to T
#'
#' @return df
#' @export
recode_chr <- function(df, col, old_names, new_name, regex = F, negate = F){

  if(regex){

  df %>%
    dplyr::pull({{col}}) %>%
    stringr::str_subset(old_names, negate = negate) %>%
    unique -> old_names}

  df %>%
    set_chr({{col}}) %>%
    dplyr::mutate({{col}} := ifelse({{col}} %in% old_names, new_name, {{col}}))
}

