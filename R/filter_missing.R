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




#' @rdname filter_missing.data.frame
#' @param remove_inf logical. default is to also remove Inf values. set to False otherwise.
#' @export
filter_missing <- function(.data, ..., remove_inf = T){

  UseMethod("filter_missing", .data)
}

#' filter out missings
#'
#' More complex wrapper around `filter(!is.na())` to remove NA rows using tidyselect. If any specified column contains an NA
#' the whole row is removed. Reports the amount of rows removed containing `NaN`, `Na`, `Inf`, in that order.
#' For example if one row contains Inf in one column and Na in another, the removed row will be counted in the Na tally.
#'
#' S3 method, can also be used on vectors
#'
#' @method filter_missing data.frame
#' @param .data dataframe
#' @param ... tidyselect. default selection is all columns
#' @param remove_inf logical. default is to also remove Inf values. set to False otherwise.
#' @param condition defaults to "any". in which case removes rows if NA is in any specified column. "all" will remove rows only if each specified column is NA
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#'
filter_missing.data.frame <- function(.data, ..., remove_inf = T, condition = c("any", "all")){

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
    warning(stringr::str_c("Removed ",nan_diff, " rows containing NaN values"), call. = F)
  }

  .data %>%
    dplyr::filter(!filter_condition(tidyselect::any_of(cols), is.na)) -> .data

  nrow(.data) -> rows3

  rows2 - rows3 -> na_diff

  if(na_diff > 0){
    warning(stringr::str_c("Removed ",na_diff, " rows containing Na values"), call. = F)
  }

  if(remove_inf){
    .data %>%
      dplyr::filter(!filter_condition(tidyselect::any_of(cols), is.infinite)) -> .data

    nrow(.data) -> rows4

    rows3 - rows4 -> inf_diff

    if(inf_diff > 0){
      warning(stringr::str_c("Removed ",inf_diff, " rows containing Inf values"), call. = F)
    }

  }

  .data
}


#' @export
filter_missing.default<- function(.data, ..., remove_inf = T){

  length(.data) -> elements1

  .data %>%
    remove_nans() -> .data

  length(.data) -> elements2

  elements1 - elements2 -> nan_diff

  if(nan_diff > 0){
    warning(stringr::str_c("Removed ",nan_diff, " elements containing NaN values"), call. = F)
  }

  .data %>%
    remove_nas()-> .data

  length(.data) -> elements3

  elements2 - elements3 -> na_diff

  if(na_diff > 0){
    warning(stringr::str_c("Removed ",na_diff, " elements containing Na values"), call. = F)
  }

  if(remove_inf){
    .data %>%
      remove_infs() -> .data

    length(.data) -> elements4

    elements3 - elements4 -> inf_diff

    if(inf_diff > 0){
      warning(stringr::str_c("Removed ",inf_diff, " elements containing Inf values"), call. = F)
    }

  }

  .data
}
