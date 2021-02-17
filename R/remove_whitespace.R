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

  if (missing(..1)) {
    .data %>%
      dplyr::select(where(is.character)) %>% names() -> db_names
  } else {
    .data %>%
      dplyr::select(...) %>% names() -> db_names
  }

  .data %>% dplyr::mutate(dplyr::across(tidyselect::any_of(db_names),  .fns = trimws))
}
