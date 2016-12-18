
setwd('~/Dropbox/Code/Stats/polynomial/')
devtools::setup(".", rstudio=FALSE)
devtools::use_build_ignore("^#")
devtools::use_testthat()
devtools::document()
