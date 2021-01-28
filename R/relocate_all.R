#' Relocate All
#'
#' Arranges columns alphabetically and then by type
#'
#' @param .data data frame
#'
#' @return data frame
#' @export
relocate_all <- function(.data){


  dplyr::relocate(.data, sort(names(.data))) %>%
    dplyr::relocate(
      where(lubridate::is.Date),
      where(is.character),
      where(is.factor),
      where(is.integer),
      where(is.numeric),
    )
}
