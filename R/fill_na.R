#' Fill NAs
#'
#' use tidyselect to fill NA values
#' Default behavior is to fill all integer or double columns cols with 0, preserving their types.
#'
#' @param .data data frame
#' @param ... tidyselect specification of columns
#' @param fill value to fill NAs
#'
#' @return data frame
#' @export
fill_na <- function(.data, ..., fill = 0L){

 .data %>%
    select_otherwise(..., otherwise = where(rlang::is_bare_double) | where(is.integer)) -> col_indx

  .data %>%
    select_otherwise(where(is.factor)) -> fct_indx

  fctrs <- dplyr::intersect(col_indx, fct_indx)


  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(setdiff(col_indx, fctrs)), ~ifelse(is.na(.), fill, .))) -> .data

  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(fctrs), ~forcats::fct_explicit_na(., na_level = as.character(fill))))
}
