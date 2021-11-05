



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

