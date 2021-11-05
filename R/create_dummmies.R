#' create dummies
#'
#' adapted from the \code{\link[fastDummies]{dummy_cols}} function Added the option to truncate the dummy column
#' names, and to specify dummy cols using tidyselect.
#'
#' reference the \href{https://jacobkap.github.io/fastDummies/index.html}{fastDummies} package for documentation on the original function.
#'
#' @param .data data frame
#' @param ... tidyselect columns. default selection is all character or factor variables
#' @param append_col_name logical, default TRUE. Appends original column name to dummy col name
#' @param max_levels uses \code{\link[forcats]{fct_lump_n}} to limit the number of categories. Only the top n levels are preserved, and the rest being lumped into "other". Default is set to 10 levels, to prevent accidental overload. Set value to \code{Inf} to use all levels
#' @param remove_first_dummy logical, default FALSE.
#' @param remove_most_frequent_dummy logical, default FALSE
#' @param ignore_na logical, default FALSE
#' @param split NULL
#' @param remove_selected_columns logical, default TRUE
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' iris %>%
#' create_dummies(Species, append_col_name = FALSE) %>%
#'   tibble::as_tibble()
#'
#'
create_dummies <- function(.data, ...,
                           append_col_name = TRUE,
                           max_levels = 10L,
                           remove_first_dummy = FALSE,
                           remove_most_frequent_dummy = FALSE,
                           ignore_na = FALSE,
                           split = NULL,
                           remove_selected_columns = TRUE){

  .data %>%
    select_otherwise(..., otherwise = where(is.character) | where(is.factor), return_type = "names") -> dummy_cols

.data %>%
  set_fct(tidyselect::any_of(dummy_cols), max_levels = max_levels) %>%
  fastDummies::dummy_cols(
    select_columns = dummy_cols,
    remove_first_dummy = remove_first_dummy,
    remove_most_frequent_dummy = remove_most_frequent_dummy,
    ignore_na = ignore_na,
    split = split,
    remove_selected_columns = remove_selected_columns
  ) -> .data1

  setdiff(names(.data1), names(.data)) -> dummynames


  if(!append_col_name){
   .data1 %>%
      dplyr::rename_with(.fn = ~stringr::str_remove(., stringr::str_c(dummy_cols, "_")), .cols = tidyselect::any_of(dummynames)) -> .data1
  }

  .data1
}

