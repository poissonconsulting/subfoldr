% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.R
\name{load_data_recursive}
\alias{load_data_recursive}
\title{Load Data Recursive}
\usage{
load_data_recursive(x, sub = get_sub(), main = get_main(),
  top = TRUE, subfolder_names = FALSE)
}
\arguments{
\item{x}{The object to save.}

\item{sub}{A string of the path to the directory to save the object.}

\item{main}{A string of the main subfolder.}

\item{top}{A flag indicating whether to include objects in the top folder.}

\item{subfolder_names}{A string indicating whether to add columns indicating subfolder names.}
}
\description{
Load Data Recursive
}
