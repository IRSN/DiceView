% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/root.R
\name{mesh_level}
\alias{mesh_level}
\title{Mesh level set of function}
\usage{
mesh_level(f, vectorized = FALSE, level = 0, intervals, mesh, ...)
}
\arguments{
\item{f}{function to be evaluated on the mesh}

\item{vectorized}{logical or function. If TRUE, f is assumed to be vectorized.}

\item{level}{level/threshold value}

\item{intervals}{matrix of intervals}

\item{mesh}{mesh object or type}

\item{...}{additional arguments passed to f}
}
\description{
Mesh level set of function
}
