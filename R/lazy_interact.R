
#' Lazy Interaction Plots
#'
#' @param xvar This variable will be displayed on the x-axis. Most often this will be categorical (i.e., a condition)
#' @param yvar This variable will be displayed on the y-axis. Most often this will be continuous (e.g., RTs)
#' @param groupby The variable to group by. For type 1 plots (Main effect plots) this is by default set to
#' match the colour-by variable. Often this needs to be a separate variable for type 2 or 3 plots.
#' @param data The dataframe where the data is stored.
#' @param colby The variable to colour by. This should be categorical (i.e., a condition). This can be different from
#' the grouping variable or the same. In type 2 or 3 plots this should often be a separate condition.
#' @param theme The theme stored as an object. By default, the built-in "InteractionTheme" is used.
#' @param linesize The size of the main effect lines, the default is set to 2.
#' @param pointsize The size of the main effect points, the default is set to 4.
#' @param position The position adjustment, by default this is set to position_dodge(width = 0.25) to avoid overlapping.
#' @param legendtitle Your desired legend title.
#' @param palette The palette to use, by default Dark2 from RColorBrewer is used.
#' @param type There are three types of plot available:
#' 1 = Main Effect plot
#' 2 = Individual data points (per Pp) plot
#' 3 = Individual slope (per Pp) plot.
#'
#' @note This function requires data in a specific format. For type 1 plots the raw data frame will suffice in most cases.
#'
#' For type 2 plots, an aggregated mean fpr each participant within relevant conditions should be used, e.g.:
#' Summary <- aggregate(RT ~ Pp  + PhraseType + Switch + Language, data = df, FUN = "mean")
#'  lazyR::lazy_interact(xvar = Summary$PhraseType,
#'                       yvar = Summary$RT,
#'                       group = Summary$Switch,
#'                       col = Summary$Switch,
#'                       data = Summary,
#'                       type = 2) +
#'  facet_wrap(~ Switch * Language)
#'
#' Similarly, type 3 plots require an aggregated mean, e.g.:
#'
#' Summary <- aggregate(RT ~ Pp  + PhraseType + Switch, data = df, FUN = "mean")
#'  lazyR::lazy_interact(xvar = Summary$PhraseType,
#'                       yvar = Summary$RT,
#'                       group = Summary$Pp,
#'                       col = Summary$Switch,
#'                       data = Summary,
#'                       type = 3) +
#'  facet_wrap(~Switch)
#'
#'
#' If you get more points or lines than desired when using type 2 or 3 plots, check that the data is in the
#' correct format - one data point per Pp per condition only.
#'
#' If the data is in the correct format, remember to apply relevant facet_wraps. In the above example,
#' the type 2 plot would need facet wrapping by Switch and Language to avoid this problem. In the second example,
#' the plot would need to be facet wrapped by Switch only. Remember that type 2 plots are more dense than type 1 plots,
#' and that type 3 plots are in turn more dense than type 2 plots. Faceted plots become increasingly difficult to scale down.
#'
#' Lastly, a note on grouping. For type 2 and type 3 plots, the group variable should be the participant ID variable,
#' while the col variable should be a discrete condition grouping variable (see the above examples)
#'
#' @return An interaction plot
#' @export
lazy_interact <- function(xvar,
                          yvar,
                          data,
                          groupby,
                          colby       = groupby,
                          shapeby     = groupby,
                          theme       = InteractTheme,
                          linesize    = 2,
                          pointsize   = 4,
                          position    = ggplot2::position_dodge(width = 0.25),
                          legendtitle = "Legend Title",
                          xlab        = "xlabel",
                          ylab        = "ylabel",
                          palette     = "Dark2",
                          type        = 1){

suppressPackageStartupMessages(library(Hmisc))

InteractTheme <- ggplot2::theme(panel.grid    = ggplot2::element_line(colour="Grey65"),
                           panel.background   = ggplot2::element_rect(fill="White"),
                           axis.ticks         = ggplot2::element_blank(),
                           axis.title.y       = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
                           axis.title.x       = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
                           strip.background   = ggplot2::element_rect(fill="white", colour="white"),
                           strip.text         = ggplot2::element_text(face="bold"),
                           axis.text          = ggplot2::element_text(size = 12),
                           axis.title         = ggplot2::element_text(size = 13),
                           panel.grid.major.x = ggplot2::element_blank(),
                           panel.grid.minor.x = ggplot2::element_blank(),
                           panel.grid.major.y = ggplot2::element_blank(),
                           panel.grid.minor.y = ggplot2::element_blank(),
                           legend.position    = "top",
                           legend.key         = ggplot2::element_rect(fill = NA))

if(type == 1) return(
  ggplot2::ggplot(data, ggplot2::aes(x     = xvar,
                                     y     = yvar,
                                     col   = colby,
                                     shape = shapeby,
                                     group = groupby)) +
  ggplot2::stat_summary(fun       = "mean",
                        geom      = "line",
                        linewidth = linesize,
                        position  = position) +
  ggplot2::stat_summary(fun.data = "mean_cl_boot",
                        geom     = "errorbar",
                        width    = 0.2,
                        position = position,
                        col = "black") +
  ggplot2::stat_summary(fun      = "mean",
                        geom     = "point",
                        size     = pointsize,
                        position = position) +
  InteractTheme +
  ggplot2::guides(shape        = ggplot2::guide_legend(title = legendtitle),
                  color        = ggplot2::guide_legend(title = legendtitle),
                  override.aes = list(linetype = 0)) +
  ggplot2::scale_color_brewer(palette = palette) +
  ggplot2::labs(x = xlab,
                y = ylab)
)

if(type == 2) return(
   ggplot2::ggplot(data, ggplot2::aes(x     = xvar,
                                      y     = yvar,
                                      col   = colby,
                                      shape = shapeby)) +
   ggbeeswarm::geom_beeswarm(ggplot2::aes(shape = shapeby),
                                          alpha = .3,
                                          cex   = 2) +
   ggplot2::stat_summary(fun       = "mean",
                         geom      = "line",
                         linewidth = linesize,
                         position  = position,
                         ggplot2::aes(group = groupby)) +
   ggplot2::stat_summary(fun      = "mean",
                         geom     = "point",
                         size     = pointsize,
                         position = position,
                         ggplot2::aes(group = groupby)) +
   InteractTheme +
   ggplot2::guides(shape        = ggplot2::guide_legend(title = legendtitle),
                   color        = ggplot2::guide_legend(title = legendtitle),
                   override.aes = list(linetype = 0)) +
   ggplot2::scale_color_brewer(palette = palette)+
    ggplot2::labs(x = xlab,
                  y = ylab)
  )

if(type == 3) return(
     ggplot2::ggplot(data, ggplot2::aes(x     = xvar,
                                        y     = yvar,
                                        col   = colby,
                                        shape = shapeby)) +
     ggplot2::geom_point(ggplot2::aes(shape = shapeby),
                         alpha = .4) +
     ggplot2::geom_line(ggplot2::aes(group = groupby)) +
     ggplot2::stat_summary(fun       = "mean",
                           geom      = "line",
                           linewidth = linesize,
                           position  = position,
                           ggplot2::aes(group = colby)) +
     ggplot2::stat_summary(fun      = "mean",
                           geom     = "point",
                           size     = pointsize,
                           position = position,
                           ggplot2::aes(group = colby)) +
     InteractTheme +
     ggplot2::guides(shape        = ggplot2::guide_legend(title = legendtitle),
                     color        = ggplot2::guide_legend(title = legendtitle),
                     override.aes = list(linetype=0)) +
     ggplot2::scale_color_brewer(palette = palette) +
      ggplot2::labs(x = xlab,
                    y = ylab)
   )
  }
