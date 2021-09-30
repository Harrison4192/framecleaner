#' select_otherwise
#'
#' flexible select operator that powers the tidy consultant universe. Used to set sensible defaults
#' and flexibly return the chosen columns. A developer focused function, but may be useful in
#' interactive programming due to the ability to return different types.
#'
#'
#' @param .data dataframe
#' @param ... tidyselect. columns to choose
#' @param otherwise tidyselect. default columns to choose if ... is not specified
#' @param col tidyselect. column to choose regardless of ... or otherwise specifications
#' @param return_type choose to return column index, names, or df. defaults to index
#'
#' @return integer vector by default. possibly data frame or character vector
#'
#'@export
#'
#'@examples
#'
#'iris %>%
#'select_otherwise(where(is.double), return_type = "index")
select_otherwise <- function(.data, ..., otherwise = NULL, col = NULL, return_type = c("names", "index", "df")){

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

