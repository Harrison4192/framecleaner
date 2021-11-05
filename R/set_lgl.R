#' set logical
#'
#' note: for non-binary data, all values other than the true_level will be set to false
#'
#' @method set_lgl data.frame
#' @param .data dataframe
#' @param ... tidyselect. Default selection: none
#' @param true_level specify the value to set as TRUE. Default value is 1 for seamless conversion between logicals and integers. Can be given as a vector of values.
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
#' # convert a 1/0 vector back into T/F
#'
#' tibble::tibble(x = c(1, 0, 0, 1, 0, 1)) %>%
#' set_lgl(x)
set_lgl.data.frame <- function(.data, ..., true_level = 1L){


  .data %>% select_otherwise(...) -> cols


  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(cols), .fns = ~set_lgl(., true_level)))
}

#' @rdname set_lgl.data.frame
#' @export
set_lgl <- function(.data, ..., true_level = 1L){

  UseMethod("set_lgl", .data)
}

#' @rdname set_lgl.data.frame
#' @method set_lgl default
#' @export
set_lgl.default <- function(.data, ...){

  .data %>%
    as.logical(...)

}


#' @rdname set_lgl.data.frame
#' @method set_lgl numeric
#' @export
set_lgl.numeric <- function(.data, ..., true_level = 1L){


    ifelse(.data %in% true_level, TRUE, FALSE)

}

#' @rdname set_lgl.data.frame
#' @method set_lgl character
#' @export
set_lgl.character <- function(.data, ..., true_level = c("T", "TRUE")){


  ifelse(.data %in% true_level, TRUE, FALSE)

}
