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


  dplyr::relocate(.data, sort(names(.data))) %>%
    dplyr::relocate(
      where(lubridate::is.Date),
      where(is.character),
      where(is.factor),
      where(is.integer),
      where(is.numeric),
    ) %>%
    dplyr::relocate(...)
}
