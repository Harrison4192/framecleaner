#' is integerish character
#'
#' companion to rlang::is_integerish that returns true only for  integerish character vectors.
#'
#' @param x a vector
#'
#' @return a logical
#' @export
#'
#' @examples
#' is_integerish_character(as.character(1:10))
#' is_integerish_character(as.character(1:5) %>% c(letters[1:5]))
#' is_integerish_character(1:10)
is_integerish_character <- function(x) {
  purrr::possibly(as.double, otherwise = "error") -> safe_dbl
  x %>% setdiff(NA) -> x1
  suppressWarnings({x1 %>% safe_dbl() -> x2})
  if(is.character(x) & rlang::is_integerish(x2)){
  !anyNA(x2)} else{
    FALSE
  }
}

#' as_integer16_or_64
#'
#' coerce to integer. if too large, coerces to 64-bit integer
#'
#' @param x integerish vec
#'
#' @return int or int64
#' @keywords internal
as_integer16_or_64 <- function(x){

  x %>% setdiff(NA) -> x1
  if(anyNA(as.integer(x1)))
    {bit64::as.integer64(x)} else{ as.integer(x)}
  }





#' auto setwd
#'
#' Call from a saved R script. Automatically sets your working directory to the directory that you saved the current
#' R script in. Takes no arguments.
#'
#' @export
auto_setwd <- function(){
  rstudioapi::getSourceEditorContext()$path %>% normalizePath %>% dirname %>% setwd
}



#' import tibble
#'
#' wrapper around [rio::import()] to return a tibble instead of a data.table
#' This is the official file reader of TidyConsultant because of it's speed, accurate
#' parsing of filetypes, int64 support, and intelligent language parsing.
#'
#' @param path filepath
#'
#' @return a tibble
#' @export
#'
import_tibble <- function(path, ...){
  rio::import(path, setclass = "tibble", ...)}

