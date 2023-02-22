#' Regression Assumption Plot Grid
#'
#' @param model A regression model (lm or lmer)
#' @param var The dependent variable used for the regression given in df$var format.
#'
#' @return A four-plot grid summarising the key assumptions of regression.
#'  Plot A shows the trend of the relationship (is it linear?)
#'  Plot B shows the variance
#'  Plot C is a histogram of resituals to look for normality
#'  Plot D is a qq plot to again look at normality of residuals.
#' @export
reg_assume <- function(model,
                       var){


  res <- stats::resid(model)
  fitted <- stats::fitted(model)
  res <- as.data.frame(cbind(res, fitted, var))
  GridTheme <- ggplot2::theme(panel.grid = ggplot2::element_line(colour="Grey65"),
                        panel.background=ggplot2::element_rect(fill="White"),
                        axis.ticks=ggplot2::element_blank(),
                        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
                        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
                        strip.background = ggplot2::element_rect(fill="white", colour="white"),
                        strip.text = ggplot2::element_text(face="bold"),
                        axis.text = ggplot2::element_text(size = 12),
                        axis.title = ggplot2::element_text(size = 13),
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor.x = ggplot2::element_blank()
                        )

  linplot <- ggplot2::ggplot(res, ggplot2::aes(x = var, y = res))
  A <- linplot +
    ggplot2::geom_point(col = "#F8766D", size = .5) +
    GridTheme +
    ggplot2::labs(x = " Observed Reaction Time Value",
                  y = "Residual Value")

  varplot <- ggplot2::ggplot(res, ggplot2::aes(x = fitted, y = res))
  B <- varplot +
    ggplot2::geom_point(col = "#00BFC4", size = .5) +
    GridTheme +
    ggplot2::labs( x = "Fitted Value",
                   y = "Residual Value") +
    ggplot2::geom_hline(yintercept = 0,
                        col        = "Black",
                        size       = 1,
                        linetype   = "dashed")

  residnorm <- ggplot2::ggplot(res, ggplot2::aes(x = res))
  C <- residnorm +
    ggplot2::geom_histogram(bins = 20,
                            fill = "Cyan",
                            col  = "Black",
                            ggplot2::aes(y = ggplot2::after_stat(density))) +
    GridTheme +
    ggplot2::labs(x = "Residual Value",
                  y = "Density") +
    ggplot2::stat_function(fun   = stats::dnorm,
                           args  = list(mean = mean(res$res),
                                        sd   = stats::sd(res$res)),
                           col   = "Red",
                           size  = 2,
                           alpha = .8)

  residqq <- ggplot2::ggplot(res, ggplot2::aes(sample = res))
  D <- residqq +
    ggplot2::geom_qq(col = "#00A9FF") +
    ggplot2::geom_qq_line(col  = "Red",
                          size = 1) +
    GridTheme +
    ggplot2::labs(x = "Theoretical Value",
                  y = "Sampled Value")

  output <- cowplot::plot_grid(A, B, C, D,
                               nrow = 2,
                               ncol = 2,
                               labels = c("A", "B", "C", "D"))
  print(output)
}
