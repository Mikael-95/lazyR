#' Generate a Pirate, Box, or Violin Plot
#'
#' @param xvar The x variable, often a discrete condition.
#' @param yvar The y variable, often a continuous variable (e.g., RT)
#' @param data The data frame containing the data
#' @param fillby Variable to fill by, by default this is set to xvar.
#' @param theme The theme to apply, by default this uses the built-in BoxTheme.
#' @param palette The palette to fill by, by default this is set to Dark2 from RColorBrewer
#' @param dodge Dodge value. By default this is set to 1. Adjust this if box- and/or violin plots do not line up.
#' @param xlab Label for the x axis
#' @param ylab Label for the y axis
#' @param type The type of plot, choose between "Pirate", "Box", and "Violin". The default is set to "Pirate".
#' @param outlier Choose whether to display outliers (boxplots only). By default these are hidden (NA)
#' @param boxwidth The width of the boxplots, adjust as needed. By default this is set to 0.5.
#'
#' @return A Pirate, box, or violinplot.
#' @export

lazy_pirate <- function(xvar,
                        yvar,
                        data,
                        fillby   = xvar,
                        theme    = BoxTheme,
                        palette  = "Dark2",
                        dodge    = 1,
                        xlab     = "xlabel",
                        ylab     = "ylabel",
                        type     = "Pirate",
                        outlier  = NA,
                        boxwidth = 0.5) {


  Box <- ggplot2::ggplot(data,
                         ggplot2::aes(xvar,
                                      yvar,
                                      fill = fillby))

  dodge <- ggplot2::position_dodge(width = dodge)

  BoxTheme <- ggplot2::theme(panel.grid         = ggplot2::element_line(colour="Grey65"),
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

  if(type == "Pirate") return(
    Box +
      ggplot2::geom_violin(position = dodge) +
      ggplot2::geom_boxplot(outlier.shape = outlier,
                            position      = dodge,
                            width         = boxwidth) +
      ggplot2::labs(x = xlab,
                    y = ylab) +
      ggplot2::theme(legend.position = "bottom",
                     aspect.ratio    = 1) +
      ggplot2::scale_fill_brewer(palette = palette) +
      BoxTheme
  )

  if(type == "Box") return(
    Box +
      ggplot2::geom_boxplot(outlier.shape = outlier,
                            position      = dodge,
                            width         = boxwidth) +
      ggplot2::labs(x = xlab,
                    y = ylab) +
      ggplot2::theme(legend.position = "bottom",
                     aspect.ratio    = 1) +
      ggplot2::scale_fill_brewer(palette = palette) +
      BoxTheme
  )

  if(type == "Violin") return(
    Box +
      ggplot2::geom_violin(position = dodge) +
      ggplot2::labs(x = xlab,
                    y = ylab) +
      ggplot2::theme(legend.position = "bottom",
                     aspect.ratio    = 1) +
      ggplot2::scale_fill_brewer(palette = palette) +
      BoxTheme
  )
}
