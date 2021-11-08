#' Make NAs
#'
#' Set elements to NA values using tidyselect specification.
#' Don't use this function on columns of different modes at once.
#' Defaults to choosing all character columns.
#'
#' @method make_na data.frame
#' @param .data data frame
#' @param ... tidyselect. Default selection: all chr cols
#' @param vec vector of possible elements to replace with NA
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' # easily set NA values. blank space and empty space are default options
#'
#' tibble::tibble(x = c("a", "b", "", "d", " ", "", "e")) %>%
#'    make_na()
#'

make_na.data.frame <- function(.data, ...,  vec = c("-", "", " ", "null", "NA", "NA_")){

  .data %>%
    select_otherwise(..., otherwise = where(is.character), return_type = "names") -> cols


  .data %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(cols), .fns = ~make_na(., vec = vec))) -> .data

  .data


}

#' @rdname make_na.data.frame
#' @export
make_na <- function(.data, ..., vec = c("-", "", " ", "null", "NA", "NA_")){

  UseMethod("make_na", .data)
}

#' @export
make_na.default<- function(.data, ..., vec = c("-", "", " ", "null", "NA", "NA_")){

  ifelse(.data %in% vec, NA, .data)
}

#' @export
#' @method make_na factor
make_na.factor <- function(.data, ..., vec = c("-", "", " ", "null", "NA", "NA_")){


  .data %>%
    as.character() %>%
    make_na.character(vec = vec) -> .data1

  .data %>%
    levels() %>%
    setdiff(vec) -> new_levls

  .data1 %>%
    factor(levels = new_levls)
}

#' @export
#' @method make_na character
make_na.character<- function(.data, ...,  vec = c("-", "", " ", "null", "NA", "NA_")){

    ifelse(.data %in% vec, NA, .data)
}

#' @export
#' @method make_na numeric
make_na.numeric <- function(.data, ..., vec = c(Inf, -Inf, NaN)){

  ifelse(.data %in% vec, NA, .data)
}
