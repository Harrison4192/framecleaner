#' Relocate All
#'
#' Arranges columns alphabetically and then by type
#' The user can supply a tidyselect argument to further specify relocations
#'
#' @param .data data frame
#' @param ... a tidyselect specification
#'
#' @return data frame
#' @export
relocate_all <- function(.data, ...){

  .data %>%
    select_otherwise(where(guess_id_col), return_type = "names") %>% sort() -> id_cols

.data %>%
    dplyr::relocate(tidyselect::any_of(id_cols)) %>%
    dplyr::relocate(where(lubridate::is.Date)) -> .data

.data

}
