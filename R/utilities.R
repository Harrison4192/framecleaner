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
#' is_integerish_character(as.character(1:5) %>% c(letters([1:5])))
#' is_integerish_character(1:10)
is_integerish_character <- function(x) {
  x %>% setdiff(NA) -> x1
  suppressWarnings({x1 %>% as.double() -> x2})
  if(is.character(x) & rlang::is_integerish(x2)){
  !anyNA(x2)} else{
    FALSE
  }
}




