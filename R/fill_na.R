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
fill_na <- function(mdb, ..., fill = 0L){

  if (missing(..1)) {
    mdb %>%
      dplyr::select(where(rlang::is_bare_double) | where(is.integer)) %>% names() -> fill_names
  } else {
    mdb %>%
      dplyr::select(...) %>% names() -> fill_names
  }

  mdb %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(fill_names), ~ifelse(is.na(.), fill, .)))
}
