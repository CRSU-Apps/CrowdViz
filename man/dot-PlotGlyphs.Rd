% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PopViz.R
\name{.PlotGlyphs}
\alias{.PlotGlyphs}
\title{Render the people on the plot.}
\usage{
.PlotGlyphs(
  plot,
  person_count,
  reference_person_count,
  treatment_person_count,
  person_spacing,
  event_desired
)
}
\arguments{
\item{plot}{{ggplot2} object to which to add elements}

\item{person_count}{Number of people being rendered}

\item{reference_person_count}{Number of people affected by the reference}

\item{treatment_person_count}{number of people affected by the treatment}

\item{person_spacing}{Spacing between people}

\item{event_desired}{}
}
\value{
{ggplot2} object
}
\description{
Render the people on the plot.
}
