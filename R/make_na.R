#' Make NAs
#'
#' Set elements to NA values using tidyselect specification.
#' Don't use this function on columns of different modes at once.
#' Defaults to choosing all character columns.
#'
#' @param .data data frame
#' @param ... tidyselect. Default selection: all chr cols
#' @param vec vector of possible elements to replace with NA
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' # easily set NA values. blank space and empty space are default options
#'
#' tibble::tibble(x = c("a", "b", "", "d", " ", "", "e")) %>%
#'    make_na()
#'

make_na <- function(.data, ...,  vec = c("-", "", " ", "null")){

  .data %>%
    select_otherwise(..., where(is.character)) -> col_indx

  .data %>%
    select_otherwise(where(is.factor)) -> fct_indx

  fctrs <- dplyr::intersect(col_indx, fct_indx)

  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(fctrs), as.character)) -> .data1



  .data1 %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(col_indx), ~ifelse(. %in% vec, NA, .))) -> .data2

  for(i in fctrs){

    .data %>%
      dplyr::pull(i) %>%
      levels() %>%
      setdiff(vec) -> new_levls

    .data2 %>%
      dplyr::mutate(dplyr::across(tidyselect::any_of(i), ~factor(., levels = new_levls))) -> .data2}

  .data2
}
