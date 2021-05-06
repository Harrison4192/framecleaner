#' Relocate All
#'
#' Arranges columns alphabetically and then by type
#' The user can supply a tidyselect argument to further specify relocations
#'
#' @param .data data frame
#' @param ... a tidyselect specification
#'
#' @return data frame
#' @export
relocate_all <- function(.data, ..., regex = NULL){

  .data %>%
    get_headers()  -> hdrs

  .data %>%
    select_otherwise(tidyselect::everything(), return_type = "names") %>% sort() -> cols

.data %>%
  dplyr::relocate(tidyselect::any_of(cols)) %>%
  dplyr::relocate(where(is.double)) %>%
  dplyr::relocate(where(is_integer)) %>%
  dplyr::relocate(where(is.factor)) %>%
  dplyr::relocate(where(is_integerish_character)) %>%
  dplyr::relocate(where(is.character)) -> d1

for(h in hdrs){
  d1 %>%
    dplyr::relocate(any_of(h) & (!where(is.double))) -> d1
}

if(!is.null(regex)){
   d1 %>%
     dplyr::relocate(tidyselect::matches(regex)) -> d1}
d1 %>%
     dplyr::relocate(where(lubridate::is.Date)) -> d2

d2

}
