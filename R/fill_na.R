#' Fill NAs
#'
#' use tidyselect to fill NA values
#'
#' @param mdb data frame
#' @param ... tidyselect specification of columns
#' @param fill value to fill NAs
#'
#' @return data frame
#' @export
fill_na <- function(mdb, ..., fill = 0){

  if (missing(..1)) {
    rlang::abort("At least one argument must be supplied")
  }

  mdb %>%
    dplyr::ungroup(.) %>%
    dplyr::mutate(dplyr::across(..., ~ifelse(is.na(.), fill, .)))
}
