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

  .data %>%
    select_otherwise(..., where(is.character)) -> col_indx

  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(col_indx), ~ifelse(. %in% vec, NA, .)))
}

