



#' @rdname set_dbl.data.frame
#' @export
set_dbl <- function(.data, ...){

  UseMethod("set_dbl", .data)
}

#' @rdname set_dbl.data.frame
#' @method set_dbl character
#' @export
set_dbl.character <- function(.data, ...){

  .data %>%
    readr::parse_number()
}

#' @rdname set_dbl.data.frame
#' @method set_dbl factor
#' @export
set_dbl.factor <- function(.data, ...){

  .data %>%
    as.character() %>%
    readr::parse_number()
}

#' @rdname set_dbl.data.frame
#' @method set_dbl Date
#' @export
set_dbl.Date <- function(.data, ...){

  .data %>%
    as.character() %>%
    stringr::str_remove_all("-") %>%
    readr::parse_number()
}

#' @rdname set_dbl.data.frame
#' @method set_dbl numeric
#' @export
set_dbl.numeric <- function(.data, ...){

  .data %>%
    as.double()
}

#' @rdname set_dbl.data.frame
#' @method set_dbl integer64
#' @export
set_dbl.integer64 <- function(.data, ...){

  .data %>%
    as.double()
}

#' set double
#'
#' @method set_dbl data.frame
#' @param .data dataframe
#' @param ... tidyselect. Default selection: none
#'
#' @return tibble
#' @export
#'
#' @examples
#'
#'
#'
#' date_col <- c(lubridate::ymd(20180101), lubridate::ymd(20210420))
#'
#'
#' tibble::tibble(int = c(1L, 2L),
#' fct = factor(c(10, 11)),
#' date = date_col,
#' chr = c("a2.1", "rtg50.5")) -> t1
#'
#' t1
#'
#' t1 %>%
#' set_dbl(tidyselect::everything())
#'
#' # s3 method works for vectors individually
#' # custom date coercion to represent date as a number. For lubridate's coercion method, use set_int
#' date_col %>%
#' set_dbl
#'
set_dbl.data.frame <- function(.data, ...){

  .data %>% select_otherwise(...)   -> cols


  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(cols), .fns = set_dbl))
}
