#' Lazy Smooth Plot
#'
#' @param xvar Variable to be displayed on the x axis
#' @param yvar Variable to be displayed on the y axis
#' @param data Data frame containing the data
#' @param colby Variable to colour by. This should be a categorical variable.
#' @param fillby Variable to fill by. This will give you the standard errors and will in most cases be the same as colby which is the default.
#' @param alpha Alkpha of the standard errors. Default is set to .15
#' @param xlab Label of the x axis
#' @param ylab Label fo the y axis
#' @param title Title of the plot
#' @param legend.title Title of the legend displayed for colby.
#' @param legend.title.fill Title of the legend displayed for fillby. By default this is the same as legend.title which will give only one legend.
#'
#' @return A smooth regression trend plot.
#' @export
lazy_smooth_plot <- function(xvar,
                             yvar,
                             data,
                             colby  = NULL,
                             fillby = colby,
                             alpha  = .15,
                             xlab   = NULL,
                             ylab   = NULL,
                             title  = NULL,
                             legend.title      = NULL,
                             legend.title.fill = legend.title){

ggplot2::ggplot(data,
                ggplot2::aes(x = xvar,
                             y = yvar)) +
    ggplot2::geom_smooth(method = "lm",
                         ggplot2::aes(col = colby,
                                      fill = fillby),
                         alpha = alpha) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x     = xlab,
                  y     = ylab,
                  title = title) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title.fill),
                    col  = ggplot2::guide_legend(title = legend.title))
}
