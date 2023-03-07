#' @rdname set_fct.data.frame
#' @export
set_fct <- function(.data, ..., first_level = NULL, order_fct = FALSE,  labels = NULL, max_levels = Inf){

  UseMethod("set_fct", .data)
}

#' set factor
#'
#' allows option to manually set the first level of the factor, for consistency with
#' yardstick which automatically considers the first level
#' as the "positive class" when evaluating classification.
#'
#' @method set_fct data.frame
#' @param .data dataframe
#' @param ... tidyselect (default selection: all character columns)
#' @param first_level character string to set the first level of the factor
#' @param labels chr vector of labels, length equal to factor levels
#' @param order_fct logical. ordered factor?
#' @param max_levels integer. uses \code{\link[forcats]{fct_lump_n}} to limit the number of categories. Only the top \code{max_levels} are preserved, and the rest being lumped into "other"
#'
#' @return tibble
#' @export
#'
#' @examples
#'
#' ## simply set the first level of a factor
#'
#' iris$Species %>% levels
#'
#' iris %>%
#'   set_fct(Species, first_level = "virginica") %>%
#'   dplyr::pull(Species) %>%
#'   levels()
set_fct.data.frame <- function(.data, ..., first_level = NULL, order_fct = FALSE,   max_levels = Inf){

  .data %>%
    select_otherwise(..., otherwise = where(is.character), return_type = "names") -> nms


  if (!is.null(first_level)) {
    first_level <- as.character(first_level)
  }



  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(nms),  .fns = ~set_fct(., first_level = first_level,
                                                                         order_fct = order_fct,
                                                                         max_levels = max_levels) ))
}



#' @rdname set_fct.data.frame
#' @method set_fct default
#' @export
set_fct.default <- function(x, first_level = NULL, order_fct = FALSE,  max_levels = Inf){


  x %>%
    factor(ordered = order_fct) %>%
    forcats::fct_relevel(first_level, after = 0L) %>%
    forcats::fct_lump(n = max_levels, ties.method = "first")
}



#' is_probability
#'
#' @param x numeric vector
#' @keywords internal
#'
#' @return logical
is_probability <- function(x){
  is.double(x) && all(dplyr::between(x, 0, 1), na.rm = T)  & dplyr::n_distinct(x) > 2
}

