#' Normal QQ Plot
#'
#' @param var The variable to sample
#' @param data The data frame containing the data
#' @param col The colour of the qq points, by default this is set to
#' "#00BFC4" which is a blue colour. Must be in "".
#' @param linecol The colour of the normal guide, by default this is set to "Red"
#' Must be in "".
#' @param linesize The width of the normal guide, by default this is set to 2.
#' @param xlab The label of the x-axis, by default this is "Theoretical Value".
#' Must be in "".
#' @param ylab The label of the y-axis, by default this is "Sampled Value".
#' Must be in "".
#'
#' @return A qq plot with a normal distribution guideline.
#' @export
lazy_qqnorm <- function(var,
                        data,
                        col      = "#00BFC4",
                        linecol  = "Red",
                        linesize = 2,
                        xlab     = "Theoretical Value",
                        ylab     = "Sampled Value"){

  QQTheme <- ggplot2::theme(panel.grid         = ggplot2::element_line(colour="Grey65"),
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

  QQ <- ggplot2::ggplot(data,
                        ggplot2::aes(sample = var))
  QQ +
    ggplot2::geom_qq(col = col) +
    ggplot2::geom_qq_line(col = linecol,
                          linewidth = linesize) +
    QQTheme +
    ggplot2::labs(x = xlab,
                  y = ylab)
}
