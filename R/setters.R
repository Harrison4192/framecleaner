

#' set type groups
#'
#'
#' @param .data dataframe
#' @param ... tidyselect. default selection is integerish doubles or integerish characters
#' @param setter which setter function to use
#' @keywords internal
#'
#' @return tibble
set_type_groups <- function(.data, ..., setter){

  .data %>%
    dplyr::groups() -> grps

  .data %>%
    dplyr::n_groups() -> n_grps1

  .data %>%
    dplyr::ungroup() -> .data

  setter(.data, ...) -> .data

  .data %>%
    dplyr::group_by(!!!grps) -> .data

  .data %>%
    dplyr::n_groups() -> n_grps2

  if(n_grps1 != n_grps2){
    warning(stringr::str_glue("number of groups was changed from {n_grps1} to {n_grps2}"), call. = F)
  }

  .data
}





# set_fct_groups <- function(.data, ..., first_level = NULL, order_fct = FALSE){
#
#   set_type_groups(.data, ..., order_fct = order_fct, first_level = first_level, setter = set_fct)
#
# }
