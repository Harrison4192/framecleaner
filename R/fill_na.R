#' Fill NAs
#'
#' use tidyselect to fill NA values
#' Default behavior is to fill all integer or double columns cols with 0, preserving their types.
#'
#' @param .data data frame
#' @param ... tidyselect specification. Default selection: none
#' @param fill value to fill NAs
#' @param missing_type character vector. Choose what type of missing to fill. Default is all types.
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' tibble::tibble(x = c(NA, 1L, 2L, NA, NaN, 5L, Inf)) -> tbl
#'
#' tbl %>%
#'   fill_na()
#'
#'tbl %>%
#'  fill_na(fill = 1L, missing_type = "Inf")
#'
#'tbl %>%
#'  fill_na(missing_type = "NaN")
#'
fill_na <- function(.data, ..., fill = 0L, missing_type = c("all", "NA", "NaN", "Inf")){

 .data %>%
    select_otherwise(..., otherwise = where(rlang::is_bare_double) | where(is.integer)) -> col_indx

  .data %>%
    select_otherwise(where(is.factor)) -> fct_indx

  fctrs <- dplyr::intersect(col_indx, fct_indx)

missing_type = match.arg(missing_type)

  if (missing_type == "all") {
    missing_condition <- function(x) {
      is.na(x) | is.infinite(x)
    }}
    else if (missing_type == "NA") {
      missing_condition <- is.na
    }
    else if (missing_type == "NaN") {
      missing_condition <- is.nan
    }
    else if (missing_type == "Inf") {
      missing_condition <- is.infinite
    }




  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(setdiff(col_indx, fctrs)), ~ifelse(missing_condition(.), fill, .))) -> .data

  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(fctrs), ~forcats::fct_explicit_na(., na_level = as.character(fill))))
}
