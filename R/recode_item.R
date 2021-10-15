#' recode_chr
#'
#' @param df data frame
#' @param col unquoted col
#' @param old_names character vector or regular expression
#' @param new_name atomic chr string
#' @param regex Logical, default F. Specify elements for old_names using a regex?
#' @param negate logical, defailt F. If negating the regex, set to T
#'
#' @return df
#' @export
#'
#' @examples
#'
#' # Use a negative regex to rename all species other than "virginica" to "none"
#'
#' iris %>%
#'    recode_chr(
#'  col = Species,
#'  old_names = "vir",
#'  new_name = "none",
#'  regex = TRUE,
#'  negate = TRUE) %>%
#'  dplyr::count(Species)
#'
#' # Specify old names using a regex
#'
#' iris %>%
#'    recode_chr(
#'    col = Species,
#'    old_names = "set|vir",
#'    new_name = "other",
#'    regex = TRUE) %>%
#'    dplyr::count(Species)
recode_chr <- function(df, col, old_names, new_name, regex = FALSE, negate = FALSE){

  if(regex){

  df %>%
    dplyr::pull({{col}}) %>%
    stringr::str_subset(old_names, negate = negate) %>%
    unique -> old_names}

  df %>%
    set_chr({{col}}) %>%
    dplyr::mutate({{col}} := ifelse({{col}} %in% old_names, new_name, {{col}}))
}

