
#' Draw a Quick and Pretty Bar Chart
#'
#' @param xvar Variable to be displayed on the X axis in df$xvar format.
#' @param yvar Variable to be displayed on the Y axis in df$yvar format.
#' @param data The data frame containing the data
#' @param palette The colour palette to use when drawing the graph.
#' @param fillby Optional. the variable to fill by.
#'               By default this is set to xvar.
#' @param width The width of each bar, by default this is set to 0.9
#' @param theme The theme to use. By default this is set to the build-in
#'              BarTheme theme.
#' @param pos Position of the bars, by default this is set to "dodge".
#' The default should be fine unless you use fill or colour to separate the data.
#' @param errorpos Position of the error bars, by default this is set to
#' position_dodge(). Replace with position_dodge(value) e.g., 0.9
#' @param xlab Text to be displayed on the x axis
#' @param ylab Text to be displayed on the y axis.
#' @return A bar chart.
#' @note
#' You can augment the function with further ggplot2 calls like
#' facet_wrap(), labs(), and theme(). If you need to limit
#' the y-axiz value to zoom in on an effect, add ggplot's
#' coord_cartesian(ylim = c(min, max)) function. You should also
#' use ggplot to hide or modify the legend.
#' @export
lazy_bar <- function(xvar,
                     yvar,
                     data,
                     palette  = "Dark2",
                     fillby   = xvar,
                     width    = 0.9,
                     theme    = BarTheme,
                     pos      = "dodge",
                     errorpos = ggplot2::position_dodge(),
                     xlab = "X Label",
                     ylab = "Y Label"){

  suppressPackageStartupMessages(library(Hmisc))
  BarTheme <- ggplot2::theme(panel.grid         = ggplot2::element_line(colour="Grey65"),
                             panel.background   = ggplot2::element_rect(fill="White"),
                             axis.ticks         = ggplot2::element_blank(),
                             axis.title.y       = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
                             axis.title.x       = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
                             strip.background   = ggplot2::element_rect(fill="white", colour="white"),
                             strip.text         = ggplot2::element_text(face="bold"),
                             axis.text          = ggplot2::element_text(size = 12),
                             axis.title         = ggplot2::element_text(size = 13),
                             panel.grid.major.x = ggplot2::element_blank(),
                             panel.grid.minor.x = ggplot2::element_blank())

  Bars <- ggplot2::ggplot(data, ggplot2::aes(x = xvar, y = yvar, fill = fillby))
  Bars +
    ggplot2::stat_summary(fun ="mean",
                          geom ="bar",
                          linewidth = 2,
                          width = width, position = pos) +
    ggplot2::stat_summary(fun.data = "mean_cl_boot",
                          geom = "errorbar",
                          width=0.2,
                          col = "black",
                          position = errorpos) +
    theme +
    ggplot2::scale_fill_brewer(palette = palette) +
    labs(x = xlab,
         y = ylab)
}
