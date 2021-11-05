
#' set date
#'
#' set dates manually or automatically
#'
#' note: can be called without any \code{...} arguments and instead automatically determines which character columns
#' are actually dates, then proceeds to set them. It checks for the date specified in \code{date_fn} and also \code{\link[lubridate]{ymd_hms}}.
#' On auto detect mode, it sets \code{\link[lubridate]{ymd_hms}} output to ymd dates instead of datetimes with hms. This is because of the common occurrence
#' of trying to extract a \code{\link[lubridate]{ymd}} date from an excel workbook, and having it come with extra 00:00:00. If you need a datetime, manually
#' supply the appropriate \href{https://lubridate.tidyverse.org/}{lubridate} function.
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
