

#' auto setwd
#'
#' Call from a saved R script. Automatically sets your working directory to the directory that you saved the current
#' R script in. Takes no arguments.
#'
#' @export
auto_setwd <- function(){
  rstudioapi::getSourceEditorContext()$path %>% normalizePath %>% dirname %>% setwd
}
