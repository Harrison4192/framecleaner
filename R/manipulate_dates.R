#' date_yq
#'
#' creates a quarterly date vector from a date vector
#'
#' @param x a date
#'
#' @return date vector
#' @export
#'
#' @examples
#'
#' seq.Date(lubridate::ymd(20200101), lubridate::ymd(20220101), length.out = 10) -> d1
#' d1 %>%
#'   tibble::enframe() %>%
#'   dplyr::mutate(YQ = date_yq(value))
date_yq <- function(x){

  lubridate::month(x) -> m

  suppressWarnings({

  m %>%
    lubridate::month() %>%
    factor %>%
    forcats::fct_collapse(
      '1' = as.character(1:3),
      '4' = as.character(4:6),
      '7' = as.character(7:9),
      '10' = as.character(10:12)) %>%
    as.character() %>%
    as.integer() -> qs

  x %>%
    lubridate::`day<-`(1L) %>%
    lubridate::`month<-`(qs)  -> x

  })

  x

}

#' date_yh
#'
#' creates a semesterly date vector from a date vector
#'
#' @param x a date
#'
#' @return date vector
#' @export
#'
#' @examples
#'
#' seq.Date(lubridate::ymd(20200101), lubridate::ymd(20220101), length.out = 10) -> d1
#' d1 %>%
#'   tibble::enframe() %>%
#'   dplyr::mutate(YH = date_yh(value))
date_yh <- function(x){

  lubridate::month(x) -> m

  suppressWarnings({

    m %>%
      lubridate::month() %>%
      factor %>%
      forcats::fct_collapse(
        '1' = as.character(1:6),
        '7' = as.character(7:12)) %>%
      as.character() %>%
      as.integer() -> qs

    x %>%
      lubridate::`day<-`(1L) %>%
      lubridate::`month<-`(qs)  -> x

  })

  x

}

#' date_ym
#'
#' creates a monthly date vector from a date vector
#'
#' @param x a date
#'
#' @return date vector
#' @export
#'
#' @examples
#'
#' seq.Date(lubridate::ymd(20200101), lubridate::ymd(20220101), length.out = 10) -> d1
#' d1 %>%
#'   tibble::enframe() %>%
#'   dplyr::mutate(YM = date_ym(value))
date_ym <- function(x){

  x %>%
    lubridate::`day<-`(1) -> x

  x
}
