#' Lazy Posterior Distribution Plots
#'
#' @param data   Required: The dataframe where the data was stored originally - this will typically be a dataframe of samples from the posterior.
#' @param xvar   Required: The variable or parameter whose posterior is to be plotted. This will often need to be put in `` in the case of interactions
#' @param ymin   Optional: This will toggle the thickness of the HDPI. Override if the defaults are too thick or thin.
#' @param ymax   Optional: This also toggles the thickness of the HDPI overlay. Override if the defaults are too thick or thin.
#' @param title  Optional: The title of the plot. The default is no title.
#' @param HDPI   Optional: The value of the HDPI. By default set to .93 to differentiate from frequentist confidence intervals.
#' @param colpos Optional: The colour of positive areas under the curve. The default is blue.
#' @param colneg Optional: The colour of negative areas under the curve. The default is a matte brown.
#'
#' @return A posterior density plot.
#' @export
lazy_post_plot <- function(data,
                           xvar,
                           ymin = -1,
                           ymax = -2,
                           title = NULL,
                           HDPI = .93,
                           colpos = "#2196F3",
                           colneg = "#A17C66"){

  ggplot2::ggplot(data    = with(stats::density(xvar),
                  base::data.frame(x, y)),
                  mapping = ggplot2::aes(x = x,
                                y = y)) +
    ggplot2::geom_area(mapping = ggplot2::aes(x = base::ifelse(x > 0, x, 0)),
                       fill    = colpos,
                       alpha   = .5) +
    ggplot2::geom_area(mapping = ggplot2::aes(x = base::ifelse(x < 0, x, 0)),
              fill    = colneg ,
              alpha   = .5) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_vline(xintercept = 0.00,
               size       = 1,
               linetype   = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Slope",
                  y = "Density",
              title = title) +
    ggplot2::annotate('rect',
             xmin = base::data.frame(coda::HPDinterval(xvar, prob = HDPI))[1, ],
             xmax = base::data.frame(coda::HPDinterval(xvar, prob = HDPI))[2, ],
             ymin = ymin,
             ymax = ymax,
             fill = "black",
             alpha = 1)
}
