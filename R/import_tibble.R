#' import tibble
#'
#' wrapper around multiple file readers. The default being \code{\link[rio]{import}} set to return a tibble
#' Also available  \code{\link[vroom]{vroom}} and \code{vroom_jp} for japanese characters.
#'
#' Supports multiple types of importing through \code{method}
#'
#' @param path filepath
#' @param ... other arguments
#' @param method method of import. default is rio
#'
#' @return a tibble
#' @export
#'
import_tibble <- function(path, ..., method = c("rio", "vroom", "vroom_jp", "read_csv", "read_excel")){

  method <- match.arg(method)

  if(method == "rio"){
  rio::import(path, setclass = "tibble", ...) -> file
    }
  else if(method == "vroom"){
    vroom::vroom(path, ...) %>% set_int() -> file
  }
  else if(method == "vroom_jp"){
    vroom::vroom(path, ..., locale = readr::locale(encoding = "shift-jis")) %>% set_int() -> file
  }
  else if(method == "read_csv"){
    readr::read_csv(path, ...) -> file
  }
  else if(method == "read_excel"){
    readxl::read_excel(path, ...) -> file
  }
  file
  }
