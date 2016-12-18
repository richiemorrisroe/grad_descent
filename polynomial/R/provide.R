
  provide <- function(package) {
      if(!require(package)) {install.packages(deparse(substitute(package)))}
      else {
          library(package)
      }
  }
