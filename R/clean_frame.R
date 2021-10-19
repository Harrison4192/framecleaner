#' Clean Data Frame
#'
#' Uses the functions of framecleaner and other operations to apply cleaning operations to a data frame
#'
#' Functions applied in \code{clean_frame}
#'
#' \itemize{
#' \item{\code{\link[janitor]{remove_empty}}}
#' \item{\code{\link[dplyr]{rename_with}} \code{.fn = \link[base]{enc2utf8}}}
#' \item{\code{\link[janitor]{clean_names}} \code{case = "all_caps", ascii = FALSE)}}
#' \item{\code{\link{set_int}}}
#' \item{\code{\link{set_date}}}
#' \item{\code{\link{make_na}}}
#' \item{\code{\link[tibble]{as_tibble}}}
#'
#' }
#'
#' @param .data a data frame
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' iris %>%
#' clean_frame()
clean_frame <- function(.data){

  suppressWarnings({

    .data %>%
      janitor::remove_empty(which = c("rows", "cols")) %>%
      dplyr::rename_with(.fn = enc2utf8) %>%
      janitor::clean_names(., case = "all_caps", ascii = FALSE) %>%
      # remove_whitespace() %>%
      set_int() %>%
      set_date() %>%
      make_na() %>%
      tibble::as_tibble()
  })
}
