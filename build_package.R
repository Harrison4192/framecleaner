xfun::gsub_dir(pattern = "frameCleaneR", replacement = "framecleaner")
# load developer libraries ------------------------------------------------


library(pacman)
p_load(rstudioapi, devtools, roxygen2, usethis, pkgdown,
       ymlthis, magrittr, fs, covr, gitcreds, credentials,
       badger, hexSticker, gh, xfun, tidyverse, r2symbols)

# create functions --------------------------------------------------------

usethis::use_r("manipulate_dates")

# add this file to .Rbuildignore ------------------------------------------


file_name  <- rstudioapi::getSourceEditorContext()$path %>% fs::path_file()
use_build_ignore(file_name)



# begin pkgdown -----------------------------------------------------------

usethis::use_pkgdown()

# create yaml -------------------------------------------------------------

ymlthis::pkgdown_template() %>%
  ymlthis::use_pkgdown_yml()

# usethis: add packages ---------------------------------------------------

usethis::use_pipe()

usethis::use_package("rstudioapi")
usethis::use_package("dplyr")
usethis::use_package("stringr")
usethis::use_package("tidyselect")
usethis::use_package("purrr")
usethis::use_package("tidyr")
usethis::use_package("janitor")
usethis::use_package("tibble")
usethis::use_package("rlang")
usethis::use_package("lubridate")
usethis::use_package("bit64")
usethis::use_package("vroom")
usethis::use_package("readxl")


usethis::use_package("badger", type = "Suggests")

usethis::use_r("create_flag")

usethis::use_r("set_fct")
usethis::use_r("set_dbl")
usethis::use_r("set_int")
usethis::use_r("set_chr")
usethis::use_r("set_date")
usethis::use_r("set_lgl")



# edit R profile ----------------------------------------------------------


edit_r_profile()



# add rmd sections with usethis -------------------------------------------

use_readme_rmd()
use_news_md()
usethis::use_mit_license()



# add badges to readme ----------------------------------------------------

use_lifecycle_badge("experimental")
use_cran_badge()
use_github_actions_badge()
# `r badger::badge_cran_download("frameCleaneR", "grand-total", "blue")`
# `r badger::badge_code_size("Harrison4192/frameCleaneR")`
# `r badger::badge_last_commit("Harrison4192/frameCleaneR")`

# set github token --------------------------------------------------------

# gh_token_help()
create_github_token()
gitcreds_set()
gitcreds_get()
set_github_pat()
# credentials::git_credential_forget()
gh::gh_whoami()
gh_token()

credentials::credential_helper_get()
git_credential_ask()
# git config --global credential.helper osxkeychain
# use github actions and links --------------------------------------------



usethis::use_github_action("check-standard")
usethis::use_github_action("test-coverage")
usethis::use_github_action("render-rmarkdown")
usethis::use_github_action("pkgdown")
usethis::use_github_actions()
usethis::use_github_links()
usethis::use_github_pages()

usethis::use_version("patch")
p_load(available)
available("validata")
# build and check ---------------------------------------------------------
devtools::find_rtools(T);Sys.getenv("PATH")
pkgbuild::has_build_tools()
devtools::document()
devtools::build_readme()
devtools::build_manual()
devtools::build_vignettes()
devtools::build_site()
devtools::check()
preview_site()
devtools::build_vignettes()
devtools::spell_check()
devtools::release(check = T)

usethis::use_cran_comments(open = rlang::is_interactive())
devtools::check_win_devel()
devtools::check_rhub()
devtools::submit_cran()
