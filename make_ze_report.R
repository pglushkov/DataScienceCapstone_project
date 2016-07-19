make_ze_report <- function()
{
  library(knitr);

  knit2html("Report.Rmd");
	browseURL("Report.html");  
}
