#' set character
#'
#' @param .data dataframe
#' @param ... tidyselect
#'
#' @return tibble
#' @export
set_chr <- function(.data, ...){

  .data %>%
    dplyr::select(...)  %>% names() -> nms


  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(nms), .fns = as.character))
}

#' set logical
#'
#' note: for non-binary data, all values other than the true_level will be set to false
#'
#' @param .data dataframe
#' @param ... tidyselect
#' @param true_level specify the value to set as TRUE. Default value is 1 for seamless conversion between logicals and integers.
#'
#' @return dataframe
#' @export
set_lgl <- function(.data, ..., true_level = 1){


    .data %>%
      dplyr::select(...)  %>% names() -> nms


  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(nms), .fns = ~ifelse(. == true_level, T, F)))
}

#' set double
#'
#' @param .data dataframe
#' @param ... tidyselect
#'
#' @return tibble
#' @export
set_dbl <- function(.data, ...){

  .data %>%
    dplyr::select(...)  %>% names() -> nms


  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(nms), .fns = as.double))
}

#' set integer
#'
#'
#'
#' @param .data dataframe
#' @param ... tidyselect. default selection is integerish doubles or integerish characters
#'
#' @return tibble
#' @export
set_int <- function(.data, ...){

  if(missing(...)) {

    .data %>%
      dplyr::select(where(rlang::is_integerish) | where(is_integerish_character))  %>% names() -> nms

  } else {

    .data %>%
      dplyr::select(...)  %>% names() -> nms  }

  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(nms), .fns = as_integer16_or_64))
}


#' set date
#'
#' set dates manually or automatically
#'
#' note: can be called without any `...` arguments and instead automatically determines which character columns
#' are actually dates, then proceeds to set them. It checks for the date specified in `date_fn` and also \code{lubridate::ymd_hms}.
#' On auto detect mode, it sets `ymd_hms` output to ymd dates instead of datetimes with hms. This is because of the common occurence
#' of trying to extract a `ymd` date from an excel workbook, and having it come with extra 00:00:00. If you need a datetime, manually
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
set_date <- function(.data, ..., date_fn = lubridate::ymd){

  if(!missing(..1)){
  .data %>%
    dplyr::select(...)  %>% names() -> nms

  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(nms), .fns = date_fn))} else {

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

#'
#' @return tibble
#' @export
set_fct <- function(.data, ..., first_level = NULL){

  if(missing(...)) {

    .data %>%
      dplyr::select(where(is.character))  %>% names() -> nms

  } else {

    .data %>%
      dplyr::select(...)  %>% names() -> nms  }


  if (!is.null(first_level)) {
    first_level <- as.character(first_level)
  }





  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(nms),
                                .fns = ~fct_or_prob(., first_level)))
}

is_probability <- function(x){
  all(dplyr::between(x, 0, 1), na.rm = T) & is.double(x) & dplyr::n_distinct(x) > 2
}


fct_or_prob <- function(x, first_level = NULL) {
  if(is_probability(x)){
    x <- ifelse(x > .5, 1, 0)
    first_level = "1"
  }
    x <-  forcats::fct_relevel(factor(x), first_level)
}

