
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
pad_auto <- function(mdb, ..., side = "left", pad = "0"){

  cols <- rlang::syms(names(dplyr::select(mdb, ...)))

  for(i in cols){

    mdb %>%
      dplyr::pull(!!i) %>%
      stringr::str_length() %>%
      max -> wth

    mdb %>%
      pad_col(!!i, width = wth, side = side, pad = pad) -> mdb
  }

  mdb
}

#' pad column
#'
#' wrapper around mutate and str_pad
#'
#' @param mdb data frame
#' @param col column
#' @param width str_pad width
#' @param pad str_pad pad
#' @param side str_pad side
#'
#' @return data frame
#' @export
pad_col <- function(mdb, col, width, pad = "0", side = "left"){

  mdb %>%
    dplyr::mutate({{col}} := as.character({{col}}) %>% stringr::str_pad(width = width, side = side, pad = pad))}
