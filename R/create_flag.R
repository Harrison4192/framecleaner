#' create flag
#'
#' @param .data data frame
#' @param col column
#' @param flag column entry
#' @param full_name Logical. default F. if T, new column name is original name + flag. other wise just flag
#' @param drop logical. default F. If T, drop original column.
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' iris %>%
#'   create_flag(
#'   col = Species,
#'   flag = "versicolor",
#'   drop = T) %>%
#'   head()
#'
create_flag <- function(.data, col, flag, full_name = F, drop = F){

  col <- rlang::ensym(col)

  if(full_name){

    nm <- stringr::str_c(rlang::as_name(col), flag, sep = "_")
  } else {
    nm <- flag
  }
  .data %>%
    dplyr::mutate("{nm}" := ifelse(!!col == flag, 1L, 0L)) -> .data

  if(drop){
    .data %>%
      dplyr::select(-!!col) -> .data
  }

  .data
}
