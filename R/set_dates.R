#' Set Dates
#'
#' Finds character columns in a data frame that represent dates and sets their type to date
#'
#' @param .data data frame
#' @param ... a tidyselect specification
#'
#' @return data frame
#' @export
set_dates <- function(.data, ...){

  if (missing(..1)) {
    rlang::abort("At least one argument must be supplied")
  }

  .data %>%
    dplyr::mutate(dplyr::across(..., as.character)) -> .data

  suppressWarnings({
    .data %>%
      dplyr::select(...) %>%
      dplyr::slice(1) -> data_slice

    purrr::map(data_slice, ~suppressWarnings(lubridate::ymd_hms(.))) %>%
      purrr::map(purrr::negate(is.na)) %>%
      purrr::map_lgl(as.logical) %>%
      which %>%
      names -> ymdhmsnames

    purrr::map(data_slice, ~suppressWarnings(lubridate::ymd(.))) %>%
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
