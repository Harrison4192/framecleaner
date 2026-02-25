

#' @rdname set_int.data.frame
#' @export
#'
#' @examples
#'
#'
#' int_vec <- c("1", "2", "10")
#'
#' tibble::tibble(
#'   chr_int = int_vec,
#'   dbl_int = c(1.0, 5.0, 20.0),
#'   chr_int64 = c("1033493932", "4432500065", "30303022192"),
#'   string_int = c("SALES2020", "SALES2021", "SALES2022")) -> tbl
#'
#' # automatically coerce integerish cols in a tibble
#' tbl
#'
#' # integerish doubles or chars will be detected for coercion automatically
#' tbl %>%
#'   set_int()
#'
#' # string_int requires parsing, so it must be specified directly for coercion
#' tbl %>%
#'   set_int(matches("str|chr"))
#'   t1 <- tibble::tibble(dt = lubridate::ymd(20250201), dttm = lubridate::now(), intg = 5L, chr = "5",
#' chr1 = "5L", chr2 = "L5")
#'
#' set_int(t1)
#'
#' # s3 method works for vectors as well
#'
#' int_vec
#'
#' int_vec %>%
#' set_int()
set_int <- function(.data, ...){

  UseMethod("set_int", .data)
}

#' set integer
#'
#'
#' @method set_int data.frame
#' @param .data dataframe
#' @param ... tidyselect. Default Selecton: integerish doubles or integerish characters
#'
#' @return tibble
#' @export
set_int.data.frame <- function(.data, ...){



  .data %>%
    select_otherwise(..., otherwise = where(is_integery)) -> cols

  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(cols), .fns = set_int)) -> .data

  .data
}

#' @rdname set_int.data.frame
#' @method set_int grouped_df
#' @export
set_int.grouped_df <- function(.data, ...){

  set_type_groups(.data, ..., setter = set_int.data.frame)
}

#' @export
set_int.default<- function(.data, ...){

  as_integer16_or_64(.data)
}

#' is integerish character
#'
#' companion to rlang::is_integerish that returns true only for  integerish character vectors.
#'
#' @param x a vector
#' @keywords internal
#'
#' @return a logical
#'
is_integerish_character <- function(x) {
  purrr::possibly(as.double, otherwise = "error") -> safe_dbl
  x %>% remove_nas() -> x1
  suppressWarnings({x1 %>% safe_dbl() -> x2})
  if(is.character(x) & rlang::is_integerish(x2)){
    !anyNA(x2)} else{
      FALSE
    }
}


#' is integery
#'
#' practical extension of rlang::is_integerish that supports integer64, integers in character strings,
#' and doesn't coerce factors
#'
#' @param x a vector
#' @keywords internal
#'
#'
#' @return logical
is_integery <- function(x){

  is_integerish_character(x) -> c1
  rlang::is_integerish(x) & !lubridate::is.Date(x) -> c2
  bit64::is.integer64(x) -> c3
  !is.factor(x) -> c4

  any(c(c1, c2, c3)) & c4
}

#' as_integer16_or_64
#'
#' coerce to integer. if too large, coerces to 64-bit integer
#'
#' @param x integerish vec
#'
#' @return int or int64
#' @export
as_integer16_or_64 <- function(x){
  if(is.character(x)){
    if(!is_integerish_character(x)) {
      message("some elements containing non-numeric values have been parsed as integers")
    }
    x <- readr::parse_number(x)
  }

  suppressWarnings({
    x %>% remove_nas() -> x1
    if(anyNA(as.integer(x1)))
    {bit64::as.integer64(x)} else{ as.integer(x)}
  })
}

