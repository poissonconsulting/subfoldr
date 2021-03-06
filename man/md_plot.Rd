% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/md.R
\name{md_plot}
\alias{md_plot}
\title{Markdown Plot}
\usage{
md_plot(x, sub = get_sub(), main = get_main(), report = get_report(),
  caption = NULL, locale = "en", ask = getOption("subfoldr.ask",
  TRUE))
}
\arguments{
\item{x}{A string of the plot name}

\item{sub}{A string of the path to the subfolders to save the object (by default = "").}

\item{main}{A string of the main subfolder.}

\item{report}{A string indicating the report folder to copy the csv files.}

\item{caption}{A string of the caption.}

\item{locale}{A string of the locale.}

\item{ask}{A string indicating whether to ask before creating a sub directory.}
}
\value{
A string of the plot in markdown format ready for inclusion in a report.
}
\description{
Returns a string of plot in markdown format ready for inclusion in a report.
}
