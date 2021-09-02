
#' pad auto
#'
#' Automatically pads elements of a column to the largest sized element. Useful when an integer code with
#' leading zeros is read in as an integer and needs to be fixed.
#'
#' @param mdb data frame
#' @param ... tidyselect specification
#' @param side str_pad side
#' @param pad str_pad pad
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' # good for putting leading 0's
#'
#' tibble::tibble(x = 1:10) %>%
#'   pad_auto(x)
pad_auto <- function(mdb, ..., side = "left", pad = "0"){

  mdb %>%
    select_otherwise(..., otherwise = where(is_integerish_character) | where(bit64::is.integer64)) -> col_indx

  for(i in col_indx){

    mdb %>%
      dplyr::pull(i) %>%
      stringr::str_length() %>%
      max -> wth

    mdb %>%
      pad_col(dplyr::any_of(i), width = wth, side = side, pad = pad) -> mdb
  }

  mdb
}

#' pad column
#'
#' wrapper around mutate and str_pad
#'
#' @param mdb data frame
#' @param ... tidyselect
#' @param width str_pad width
#' @param pad str_pad pad
#' @param side str_pad side
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#'  # manually pad with 0's (or other value)
#'  # use case over [pad_auto()]: the desired width is greater than the widest element
#'
#'  tibble::tibble(
#'  ID = c(2, 13, 86, 302)
#'  ) %>%
#'  pad_col(ID, width = 4)
pad_col <- function(mdb, ..., width, pad = "0", side = "left"){

  mdb %>%
    select_otherwise(...) -> cols

  mdb %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(cols),  .fns =  ~stringr::str_pad(as.character(.), width = width, side = side, pad = pad)))}
