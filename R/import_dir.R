#' import directory
#'
#' @param dir dir path
#' @param ... arguments passed to import method
#' @param method import method chosen from import tibble
#' @param return_type default is to bind dataframes together and remove duplicates. only recommended
#' for a folder of files with the same data format. otherwise specify return as list of data frames
#'
#' @return data frame
#' @export
#'
import_dir <-function(dir,
                      ...,
                      method = c("rio", "vroom", "vroom_jp", "read_csv"),
                      return_type = c("df", "list")) {
  method1 <- match.arg(method)
  return_type1 <- match.arg(return_type)

  if(!fs::is_dir(dir)){rlang::abort("dir must be a file directory path")}

  fs::dir_ls(dir) -> files
  file_list <- list()

  for (f in seq_along(files)) {
    import_tibble(files[f], ..., method = method1)  -> file_list[[f]]
  }

  if(return_type1 == "df"){
  rlist::list.rbind(file_list) %>%
    dplyr::distinct(.) -> file_list}

  file_list
}
