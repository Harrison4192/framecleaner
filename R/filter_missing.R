#' remove nas
#'
#' @param x vec
#' @keywords internal
#'
#' @return vec
#'
remove_nas <- function(x){

  x[which(!is.na(x))]
}

#' remove nans
#'
#' @param x vec
#' @keywords internal
#'
#' @return vec
#'
remove_nans <- function(x){

  x[which(!is.nan(x))]
}
#' remove infs
#'
#' @param x vec
#' @keywords internal
#'
#' @return vec
#'
remove_infs <- function(x){

  x[which(!is.infinite(x))]
}

#' is missing or inf
#'
#' @param x
#'
#' @return logical
#' @keywords internal
#'
is_missing_or_inf <- function(x){

  is.na(x) | is.infinite(x)
}



#' @rdname filter_missing.data.frame
#' @param remove_inf logical. default is to also remove \code{Inf} values. set to \code{FALSE} otherwise.
#' @export
filter_missing <- function(.data, ..., remove_inf = TRUE){

  UseMethod("filter_missing", .data)
}

#' filter out missings
#'
#' More complex wrapper around \code{dplyr::filter(!is.na())} to remove \code{NA} rows using tidyselect. If any specified column contains an \code{NA}
#' the whole row is removed. Reports the amount of rows removed containing \code{NaN}, \code{NA}, \code{Inf}, in that order.
#' For example if one row contains \code{Inf} in one column and  in another, the removed row will be counted in the \code{NA} tally.
#'
#' S3 method, can also be used on vectors
#'
#' @method filter_missing data.frame
#' @param .data dataframe
#' @param ... tidyselect. default selection is all columns
#' @param remove_inf logical. default is to also remove \code{Inf} values. set to \code{FALSE} otherwise.
#' @param condition defaults to "any". in which case removes rows if \code{NA} is in any specified column. "all" will remove rows only if each specified column is missing
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' tibble::tibble(x = c(NA, 1L, 2L, NA, NaN, 5L, Inf),
#' y = c(1L, NA, 2L, NA, Inf, 5L, Inf)) -> tbl1
#'
#' tbl1
#'
#' # remove any row with a missing or Inf
#' tbl1 %>%
#' filter_missing()
#'
#' # remove any row with Na or NaN in the x column
#' tbl1 %>%
#' filter_missing(x, remove_inf = FALSE)
#'
#' # only remove rows where every entry is Na, NaN, or Inf
#' tbl1 %>%
#' filter_missing(condition = "all")
filter_missing.data.frame <- function(.data, ..., remove_inf = TRUE, condition = c("any", "all")){

  condition <- match.arg(condition)

  if(condition == "any"){
    filter_condition <- dplyr::if_any
  } else {
    filter_condition <- dplyr::if_all

  }


  .data %>%
    select_otherwise(..., otherwise = tidyselect::everything()) -> cols

  nrow(.data) -> rows1

  .data %>%
    dplyr::filter(!filter_condition(tidyselect::any_of(cols), is.nan)) -> .data

  nrow(.data) -> rows2

  rows1 - rows2 -> nan_diff

  if(nan_diff > 0){
    warning(stringr::str_c("Removed ",nan_diff, " rows containing NaN values"), call. = FALSE)
  }

  .data %>%
    dplyr::filter(!filter_condition(tidyselect::any_of(cols), is.na)) -> .data

  nrow(.data) -> rows3

  rows2 - rows3 -> na_diff

  if(na_diff > 0){
    warning(stringr::str_c("Removed ",na_diff, " rows containing NA values"), call. = FALSE)
  }

  if(remove_inf){
    .data %>%
      dplyr::filter(!filter_condition(tidyselect::any_of(cols), is_missing_or_inf)) -> .data

    nrow(.data) -> rows4

    rows3 - rows4 -> inf_diff

    if(inf_diff > 0){
      warning(stringr::str_c("Removed ",inf_diff, " rows containing Inf values"), call. = FALSE)
    }

  }

  .data
}


#' @export
filter_missing.default<- function(.data, ..., remove_inf = TRUE){

  length(.data) -> elements1

  .data %>%
    remove_nans() -> .data

  length(.data) -> elements2

  elements1 - elements2 -> nan_diff

  if(nan_diff > 0){
    warning(stringr::str_c("Removed ",nan_diff, " elements containing NaN values"), call. = FALSE)
  }

  .data %>%
    remove_nas()-> .data

  length(.data) -> elements3

  elements2 - elements3 -> na_diff

  if(na_diff > 0){
    warning(stringr::str_c("Removed ",na_diff, " elements containing NA values"), call. = FALSE)
  }

  if(remove_inf){
    .data %>%
      remove_infs() -> .data

    length(.data) -> elements4

    elements3 - elements4 -> inf_diff

    if(inf_diff > 0){
      warning(stringr::str_c("Removed ",inf_diff, " elements containing Inf values"), call. = FALSE)
    }

  }

  .data
}
