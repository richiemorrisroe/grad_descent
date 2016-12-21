
setwd('~/Dropbox/Code/Stats/polynomial/')
devtools::use_build_ignore("^#")
devtools::use_testthat()
devtools::document()
devtools::build()
devtools::check()
devtools::install()
