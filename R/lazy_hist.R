#' Easily Create a Pretty Histogram
#'
#' @param xvar The variable to be displayed on the x axis in df$xvvar format
#' @param data The data frame where the data resides
#' @param theme Optional - an object containing a theme.
#'              By default, the built-in HistTheme is used.
#' @param col Optional - The outline colour of each bar.
#'              By default this is black.
#' @param fill Optional - The fill colour of each bar.
#'               By default this is cyan.
#' @param bins Optional - The desired number of bins.
#'             By default this is 30.
#' @param normal Toggle whether to display a normal curve overlay.
#'               By default, this is set to TRUE.
#'
#' @return A histogram
#' @note This function creates a ggplot2 object. You can further edit this by
#' using normal ggplot2 syntax. For example, to wrap you could add
#' lazyR::lazy_hist(xvar, df) + facet_wrap(~cond1 * cond2)
#' to create a 2x2 histogram grid.
#' @export
lazy_hist <- function(xvar, data, theme = HistTheme, col = "black", fill = "cyan", bins = 30, normal = TRUE){

  HistTheme <- ggplot2::theme(panel.grid         = ggplot2::element_line(colour = "Grey65"),
                              panel.background   = ggplot2::element_rect(fill = "White"),
                              axis.ticks         = ggplot2::element_blank(),
                              axis.title.y       = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
                              axis.title.x       = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
                              strip.background   = ggplot2::element_rect(fill = "white", colour="white"),
                              strip.text         = ggplot2::element_text(face = "bold"),
                              axis.text          = ggplot2::element_text(size = 12),
                              axis.title         = ggplot2::element_text(size = 13),
                              panel.grid.major.x = ggplot2::element_blank(),
                              panel.grid.minor.x = ggplot2::element_blank())

  Histogram <- ggplot2::ggplot(data, ggplot2::aes(x = xvar))

  if(normal == FALSE) return(
    Histogram +
      ggplot2::geom_histogram(col = col,
                              fill = fill,
                              bins = bins) +
      ggplot2::labs(y = "Count") +
      theme)
  if(normal == TRUE) return(
    Histogram +
      ggplot2::geom_histogram(col = col,
                              fill = fill,
                              bins = bins,
                              ggplot2::aes(y = ggplot2::after_stat(density))) +
      ggplot2::labs(y = "Density") +
      theme +
      ggplot2::stat_function(fun = stats::dnorm,
                             args = list(mean = mean(xvar), sd = stats::sd(xvar)),
                             col = "Red",
                             linewidth = 2,
                             alpha = .8))
}
