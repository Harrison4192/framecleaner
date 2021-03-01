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
      set_int() %>%
      set_date() %>%
      remove_whitespace() %>%
      make_na() %>%
      relocate_all(.)


  })
}
