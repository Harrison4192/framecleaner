#' set character
#'
#' @param .data dataframe
#' @param ... tidyselect. Default selection: none
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
#' iris %>%
#' tibble::as_tibble() %>%
#' set_chr(tidyselect::everything())
#'
set_chr <- function(.data, ...){

  .data %>% select_otherwise(...) -> cols


  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(cols), .fns = as.character))
}
