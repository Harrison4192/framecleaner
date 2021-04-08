#' Clean Data Frame
#'
#' Uses the functions of frameCleaneR and other operations to apply cleaning operations to a data frame
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
      remove_whitespace() %>%
      set_int() %>%
      set_date() %>%
      make_na() %>%
      tibble::as_tibble()
  })
}
