#' set character
#'
#' @param .data dataframe
#' @param ... tidyselect. Default selection: none
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
#' iris %>%
#' tibble::as_tibble() %>%
#' set_chr(tidyselect::everything())
#'
set_chr <- function(.data, ...){

  .data %>% select_otherwise(...) -> cols


  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(cols), .fns = as.character))
}

#' set logical
#'
#' note: for non-binary data, all values other than the true_level will be set to false
#'
#' @param .data dataframe
#' @param ... tidyselect. Default selection: none
#' @param true_level specify the value to set as TRUE. Default value is 1 for seamless conversion between logicals and integers.
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
set_lgl <- function(.data, ..., true_level = 1L){


    .data %>% select_otherwise(...) -> cols


  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(cols), .fns = ~ifelse(. == true_level, T, F)))
}

#' @rdname set_dbl.data.frame
#' @export
set_dbl <- function(.data, ...){

  UseMethod("set_dbl", .data)
}

#' @rdname set_dbl.data.frame
#' @method set_dbl character
#' @export
set_dbl.character <- function(x){

  x %>%
    readr::parse_number()
}

#' @rdname set_dbl.data.frame
#' @method set_dbl factor
#' @export
set_dbl.factor <- function(x){

  x %>%
    as.character() %>%
    readr::parse_number()
}

#' @rdname set_dbl.data.frame
#' @method set_dbl Date
#' @export
set_dbl.Date <- function(x){

  x %>%
    as.character() %>%
    stringr::str_remove_all("-") %>%
    readr::parse_number()
}

#' @rdname set_dbl.data.frame
#' @method set_dbl numeric
#' @export
set_dbl.numeric <- function(x){

  x %>%
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

#' @rdname set_int.data.frame
#' @export
#'
#' @examples
#'
#' # automaticaly turns "integerish" characters or doubles into int values
#'
#'int_vec <- c("1", "2", "10")
#'
#' tibble::tibble(
#' x = int_vec,
#' y = c(1.0, 5.0, 20.0)) -> tbl
#'
#' # automatically coerce integerish cols in a tibble
#' tbl
#'
#' tbl %>%
#' set_int()
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

#' @method set_int grouped_df
#' @export
set_int.grouped_df <- function(.data, ...){

  set_type_groups(.data, ..., setter = set_int.data.frame)
}

#' @export
set_int.default<- function(.data, ...){

  as_integer16_or_64(.data)
}

#' set type groups
#'
#'
#' @param .data dataframe
#' @param ... tidyselect. default selection is integerish doubles or integerish characters
#' @param setter which setter function to use
#' @keywords internal
#'
#' @return tibble
set_type_groups <- function(.data, ..., setter){

  .data %>%
    dplyr::groups() -> grps

  .data %>%
    dplyr::n_groups() -> n_grps1

  .data %>%
    dplyr::ungroup() -> .data

  setter(.data, ...) -> .data

  .data %>%
    dplyr::group_by(!!!grps) -> .data

  .data %>%
    dplyr::n_groups() -> n_grps2

  if(n_grps1 != n_grps2){
    warning(stringr::str_glue("number of groups was changed from {n_grps1} to {n_grps2}"), call. = F)
  }

  .data
}


#' set date
#'
#' set dates manually or automatically
#'
#' note: can be called without any \code{...} arguments and instead automatically determines which character columns
#' are actually dates, then proceeds to set them. It checks for the date specified in \code{date_fn} and also \code{\link[lubridate]{ymd_hms}}.
#' On auto detect mode, it sets \code{\link[lubridate]{ymd_hms}} output to ymd dates instead of datetimes with hms. This is because of the common occurrence
#' of trying to extract a \code{\link[lubridate]{ymd}} date from an excel workbook, and having it come with extra 00:00:00. If you need a datetime, manually
#' supply the appropriate lubridate function.
#'
#' Auto mode is experimental. Commonly detected error is a long character string of integers being interpreted as a date.
#'
#' @param .data dataframe
#' @param ... tidyselect
#' @param date_fn a function to convert to a date object
#'
#' @return tibble
#' @export
#'
#' @examples
#'
#' tibble::tibble(date_col1 = c("20190101", "20170205"),
#' date_col2 = c("20201015", "20180909"),
#' not_date_col = c("a345", "b040")) -> t1
#'
#' t1
#'
#' t1 %>%
#' set_date()
#'
#' t1 %>%
#' set_date(date_col1)
set_date <- function(.data, ..., date_fn = lubridate::ymd){


  if(!missing(..1)){
  .data %>% select_otherwise(..., return_type = "names") -> nms


  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(nms), .fns = ~date_fn(.)))} else {

      .data %>%
        dplyr::select(where(is.character)) %>% names() -> fill_names

      suppressWarnings({
        .data %>%
          dplyr::select(tidyselect::any_of(fill_names)) %>%
          dplyr::slice(1) -> data_slice

        purrr::map(data_slice, ~suppressWarnings(lubridate::ymd_hms(.))) %>%
          purrr::map(purrr::negate(is.na)) %>%
          purrr::map_lgl(as.logical) %>%
          which %>%
          names -> ymdhmsnames

        purrr::map(data_slice, ~suppressWarnings(date_fn(.))) %>%
          purrr::map(purrr::negate(is.na)) %>%
          purrr::map_lgl(as.logical) %>%
          which %>%
          names -> ymdnames


        if(!rlang::is_empty(ymdnames)){
          .data %>%
            dplyr::mutate(dplyr::across(tidyselect::any_of(ymdnames), lubridate::as_date)) -> .data}

        if(!rlang::is_empty(ymdhmsnames)){
          .data %>%
            dplyr::mutate(dplyr::across(tidyselect::any_of(ymdhmsnames), lubridate::as_date)) -> .data}


        if (!rlang::is_empty(ymdnames) | !rlang::is_empty(ymdhmsnames)) {
          .data %>%
            dplyr::select(where(lubridate::is.Date)) %>%
            dplyr::slice(1) %>%
            purrr::map_lgl( ~ !dplyr::between(lubridate::year(.), lubridate::year(lubridate::now()) - 50, lubridate::year(lubridate::now()) + 50)) -> lg1
          any(lg1) -> if_true_error

          stringr::str_c(names(lg1[which(lg1)]), collapse = ", ") -> nms

          if(if_true_error){rlang::abort(stringr::str_glue("function created erroneous dates for {nms}"))}

        }

      })
      .data
    }
}

#' set factor
#'
#' allows option to manually set the first level of the factor, for consistency with
#' yardstick which automatically considers the first level
#' as the "positive class" when evaluating classification.
#'
#'
#' @param .data dataframe
#' @param ... tidyselect (default selection: all character columns)
#' @param first_level character string to set the first level of the factor
#' @param order_fct logical. ordered factor?
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
set_fct <- function(.data, ..., first_level = NULL, order_fct = FALSE){

  .data %>%
    select_otherwise(..., otherwise = where(is.character)) -> nms


  if (!is.null(first_level)) {
    first_level <- as.character(first_level)
  }

  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(nms),
                                .fns = ~fct_or_prob(., first_level = first_level, order_fct = order_fct)))
}


# set_fct_groups <- function(.data, ..., first_level = NULL, order_fct = FALSE){
#
#   set_type_groups(.data, ..., order_fct = order_fct, first_level = first_level, setter = set_fct)
#
# }
