#' Remove Whitespace
#'
#' Remove whitespace from columns using a tidyselect specification.
#'
#' @param .data data frame
#' @param ... tidyselect specification (default selection:  all character columns)
#'
#' @return data frame
#' @export
remove_whitespace <- function(.data, ...){

  .data %>%
    select_otherwise(..., otherwise = where(is.character)) -> col_indx

  .data %>% dplyr::mutate(dplyr::across(tidyselect::any_of(col_indx),  .fns = trimws))
}
