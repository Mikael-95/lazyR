#' Dark-Theme Friendly Kable
#'
#' @param ...  see the documentation for knitr::kable
#' @param format A character string denoting format. This is set to "html" by default.
#' @param digits The number of decimals to print, byd efault this is set to 2.
#' @param table.attr By default this is set to "style = \"color:white;\"" which together with format = html gives
#' white text output in RMarkdown.
#'
#' @return A kable-formated table using html styling to achieve white text.
#' @export
#'
#' @note See ?knitr::kable for more details on each argument. In order to get black text on a white background when
#' knitting, the following CSS code must be added to the start of the RMarkdown file (and NOT in an R code chunk):
#'
#' <style>
#' table {
#'  background-color: white !important;
#'  color: black !important;
#'}
#'</style>

dark_kable <- function(...) {
  knitr::kable(..., format = "html", table.attr = "style = \"color:white;\"", digits = 2)
}

