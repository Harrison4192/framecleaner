
# load developer libraries ------------------------------------------------



library(pacman)
p_load(rstudioapi, devtools, roxygen2, usethis, pkgdown,
       ymlthis, magrittr, fs, covr, gitcreds, credentials,
       badger, hexSticker)


# add this file to .Rbuildignore ------------------------------------------


file_name  <- rstudioapi::getSourceEditorContext()$path %>% fs::path_file()
use_build_ignore(file_name)


# edit R profile ----------------------------------------------------------


edit_r_profile()



# add rmd sections with usethis -------------------------------------------

use_readme_rmd()
use_news_md()


# add badges to readme ----------------------------------------------------

use_binder_badge()
use_lifecycle_badge("experimental")
use_cran_badge()
use_github_actions_badge()



# set github token --------------------------------------------------------

# gh_token_help()
create_github_token()
# 6a76116825804263c2e47970ed369f99f9fc71f3
gitcreds_set()
gitcreds_get()
set_github_pat()
# credentials::git_credential_forget()
gh::gh_whoami()


# use github actions and links --------------------------------------------



usethis::use_github_action("check-standard")
usethis::use_github_action("test-coverage")
usethis::use_github_action("render-rmarkdown")
usethis::use_github_action("pkgdown")
usethis::use_github_actions()
usethis::use_github_links()



# build and check ---------------------------------------------------------


build_readme()
build_site()
check()


