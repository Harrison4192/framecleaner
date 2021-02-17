#' Make NAs
#'
#' Set elements to NA values using tidyselect specification.
#' Don't use this function on columns of different modes at once.
#' Defaults to choosing all character columns.
#'
#' @param .data data frame
#' @param ... tidyselect specification
#' @param vec vector of possible elements to replace with NA
#'
#' @return data frame
#' @export

make_na <- function(.data, ...,  vec = c("-", "", " ", "null")){

  if (missing(..1)) {
    .data %>%
      dplyr::select(where(is.character)) %>% names() -> db_names
  } else {
    .data %>%
      dplyr::select(...) %>% names() -> db_names
  }

  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(db_names), ~ifelse(. %in% vec, NA, .)))
}

