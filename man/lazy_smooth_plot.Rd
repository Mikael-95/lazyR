% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lazy_smooth_plot.R
\name{lazy_smooth_plot}
\alias{lazy_smooth_plot}
\title{Lazy Smooth Plot}
\usage{
lazy_smooth_plot(
  xvar,
  yvar,
  data,
  colby = NULL,
  fillby = colby,
  alpha = 0.15,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  legend.title = NULL,
  legend.title.fill = legend.title
)
}
\arguments{
\item{xvar}{Variable to be displayed on the x axis}

\item{yvar}{Variable to be displayed on the y axis}

\item{data}{Data frame containing the data}

\item{colby}{Variable to colour by. This should be a categorical variable.}

\item{fillby}{Variable to fill by. This will give you the standard errors and will in most cases be the same as colby which is the default.}

\item{alpha}{Alkpha of the standard errors. Default is set to .15}

\item{xlab}{Label of the x axis}

\item{ylab}{Label fo the y axis}

\item{title}{Title of the plot}

\item{legend.title}{Title of the legend displayed for colby.}

\item{legend.title.fill}{Title of the legend displayed for fillby. By default this is the same as legend.title which will give only one legend.}
}
\value{
A smooth regression trend plot.
}
\description{
Lazy Smooth Plot
}
