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





#' auto setwd
#'
#' Call from a saved R script. Automatically sets your working directory to the directory that you saved the current
#' R script in. Takes no arguments.
#'
#' @return No return value.
#' @export
#'
auto_setwd <- function(){

  rstudioapi::getSourceEditorContext()$path %>% normalizePath %>% dirname %>% setwd

  getwd() -> newwd

  message(stringr::str_c("Working directory set to ", newwd))
}







#' is_probability
#'
#' @param x numeric vector
#' @keywords internal
#'
#' @return logical
is_probability <- function(x){
   is.double(x) && all(dplyr::between(x, 0, 1), na.rm = T)  & dplyr::n_distinct(x) > 2
}


#' fct_or_prob
#'
#' @param x vector
#' @param first_level character string to set the first level of the factor
#' @param order_fct logical. ordered factor?
#' @keywords internal
#'
#' @return logical
fct_or_prob <- function(x, first_level = NULL, order_fct = FALSE) {
  if(is_probability(x)){
    x <- ifelse(x > .5, 1, 0)
  }
  x <-  forcats::fct_relevel(factor(x, ordered = order_fct), first_level)

  x
}

get_headers <- function(db){

  db %>%
    names() %>%
    enc2utf8() -> nms

  nms %>%
    stringr::str_extract("^.*(?=(_|\\.))") %>%
    table() %>%
    subset(subset = . > 1) %>%
    names() -> hdrs



  if(rlang::is_empty(hdrs)){
    nms1 <- nms
  } else{
    nms %>%
      stringr::str_subset(stringr::str_c(hdrs, collapse = "|"), negate = T) -> nms1

  }

  nms1 %>%
    stringr::str_extract("(?<=(_|\\.)).*$") %>%
    table() %>%
    subset(subset = . > 1) %>%
    names() -> hdrs1

  unique(c(hdrs1, hdrs))

}

