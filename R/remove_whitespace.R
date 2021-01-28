#' Remove Whitespace
#'
#' Remove whitespace from columns using a tidyselect specification.
#'
#' @param .data data frame
#' @param ... tidyselect specification
#'
#' @return data frame
#' @export
remove_whitespace <- function(.data, ...){

  if (missing(..1)) {
    rlang::abort("At least one argument must be supplied")
  }

  .data %>% dplyr::mutate(dplyr::across(...,  .fns = trimws))
}
