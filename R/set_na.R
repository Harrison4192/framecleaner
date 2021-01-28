#' Set NAs
#'
#' Set elements to NA values using tidyselect specification.
#' Don't use this function on columns of different modes at once.
#'
#' @param .data data frame
#' @param ... tidyselect specification
#' @param vec vector of elements to replace with NA
#'
#' @return data frame
#' @export

set_na <- function(.data, ...,  vec = c("-", "", " ", "null")){

  if (missing(..1)) {
    rlang::abort("At least one argument must be supplied")
  }

  .data %>%
    dplyr::mutate(dplyr::across(..., ~ifelse(. %in% vec, NA, .)))
}
