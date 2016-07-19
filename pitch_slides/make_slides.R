make_slides <- function() {

  library(slidify)
  slidify('index.Rmd')

  library(knitr)
  browseURL('index.html')
}
