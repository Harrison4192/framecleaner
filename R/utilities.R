#' is integerish character
#'
#' companion to rlang::is_integerish that returns true only for  integerish character vectors.
#'
#' @param x a vector
#' @keywords internal
#'
#' @return a logical
#' @export
#' @keywords internal
#'
#' @examples
#' is_integerish_character(as.character(1:10))
#' is_integerish_character(as.character(1:5) %>% c(letters[1:5]))
#' is_integerish_character(1:10)
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
#' @export
is_integery <- function(x){

  is_integerish_character(x) -> c1
  rlang::is_integerish(x) -> c2
  bit64::is.integer64(x) -> c3
  !is.factor(x) -> c4

  any(c(c1, c2, c3)) & c4
}

#' as_integer16_or_64
#'
#' coerce to integer. if too large, coerces to 64-bit integer
#'
#' @param x integerish vec
#' @keywords internal
#'
#' @return int or int64
#' @keywords internal
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

#' Title
#'
#' @param x
#' @keywords internal
#'
#' @return
#' @export
#'
#' @examples
remove_nas <- function(x){

  x[which(!is.na(x))]
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
#' @param ... other arguments
#'
#' @return a tibble
#' @export
#'
import_tibble <- function(path, ...){
  rio::import(path, setclass = "tibble", ...)}


#' @param .data dataframe
#' @param ... tidyselect
#' @param otherwise tidyselect
#' @param col tidyselect
#' @param return_type choose to return column index, names, or df. defaults to index
#'
#' @return integer vector by default. possibly data frame or character vector
#' @keywords internal
#'
select_otherwise <- function(.data, ..., otherwise = NULL, col = NULL, return_type = c("index", "names", "df")){

  return_type <- match.arg(return_type)

  .dots <- rlang::expr(c(...))

  col <- rlang::enexpr(col)

  otherwise = rlang::enexpr(otherwise)


  tidyselect::eval_select(.dots, data = .data) -> eval1

  if(length(eval1) == 0){
    tidyselect::eval_select( otherwise, data = .data) -> eval1
  }

  tidyselect::eval_select(col, data = .data) %>%
    c(eval1) %>% sort() -> eval1


  if(return_type == "df"){

    out <- .data %>% dplyr::select(tidyselect::any_of(eval1))
  } else if(return_type == "names"){
    out <- names(eval1)
  } else{
    out <- eval1
  }

  out
}




#' is_probability
#'
#' @param x numeric vector
#'
#' @return logical
#' @keywords internal
#'
is_probability <- function(x){
   is.double(x) && all(dplyr::between(x, 0, 1), na.rm = T)  & dplyr::n_distinct(x) > 2
}


#' fct_or_prob
#'
#' @param x vector
#' @param first_level character string to set the first level of the factor
#' @param order_fct logical. ordered factor?
#'
#' @return logical
#' @keywords internal
#'
fct_or_prob <- function(x, first_level = NULL, order_fct = FALSE) {
  if(is_probability(x)){
    x <- ifelse(x > .5, 1, 0)
  }
  x <-  forcats::fct_relevel(factor(x, ordered = order_fct), first_level)

  x
}



