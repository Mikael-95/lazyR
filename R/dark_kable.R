#' Dark-Theme Friendly Kable
#'
#' @param x The data to be printed as a kable, typically a matrix or a dataframe.
#' @param format A character string denoting format. This is set to "html" by default.
#' @param digits The number of decimals to print, byd efault this is set to 2.
#' @param row.names Logical: whether to include row names.
#' @param col.names A character vector of column names to be used in the table.
#' @param align Alignment of cell contents
#' @param caption The caption of the table
#' @param label The table reference label
#' @param format.args  A list of arguments to be passed to format
#' @param escape Boolean; whether to escape special characters when producing html or LaTeX tables.
#' @param decimal.mark The symbol to be used for decimals, by default this is set to "."
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
#'
#'This code is also stored in the lazyR package as enable_dark_kable for easy reference.
#'NB! The code must still be copied in, calling the object will not work.

dark_kable <- function(x,
                       format = "html",
                       digits = 2,
                       row.names = NA,
                       col.names = NA,
                       align = NULL,
                       caption = NULL,
                       label = NULL,
                       format.args = list(),
                       escape = TRUE,
                       decimal.mark = ".",
                       table.attr = "style = \"color:white;\""){
  knitr::kable(x = x,
              format = format,
              digits = digits,
              row.names = row.names,
              col.names = col.names,
              align = align,
              caption = caption,
              label = label,
              format.args = format.args,
              escape = escape,
              decimal.mark = decimal.mark)
}

enable_dark_kable <- "<style>
  table {
    background-color: white !important;
    color: black !important;
  }
</style>"
