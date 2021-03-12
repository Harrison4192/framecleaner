#' Fill NAs
#'
#' use tidyselect to fill NA values
#' Default behavior is to fill all integer or double columns cols with 0, preserving their types.
#'
#' @param mdb data frame
#' @param ... tidyselect specification of columns
#' @param fill value to fill NAs
#'
#' @return data frame
#' @export
fill_na <- function(.data, ..., fill = 0L){

 .data %>%
    select_otherwise(..., otherwise = where(rlang::is_bare_double) | where(is.integer)) -> fill_names

  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(fill_names), ~ifelse(is.na(.), fill, .)))
}
