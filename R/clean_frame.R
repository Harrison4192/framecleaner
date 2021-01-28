#' Clean Data Frame
#'
#' Uses the functions of dataCleaner and other operations to apply cleaning operations to a data frame
#'
#' @param .data a data frame
#'
#' @return data frame
#' @export
clean_frame <- function(.data){

  suppressWarnings({

    .data %>%
      janitor::remove_empty(which = c("rows", "cols")) %>%
      janitor::clean_names(., case = "all_caps", ascii = F) %>%
      dplyr::mutate(dplyr::across(where(rlang::is_integerish), as.integer)) %>%
      dataCleaner::set_dates(tidyselect::everything()) %>%
      dataCleaner::remove_whitespace(where(is.character)) %>%
      dataCleaner::set_na(where(is.character)) %>%
      dataCleaner::relocate_all(.)


  })
}
