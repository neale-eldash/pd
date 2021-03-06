% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampling_raking.R
\name{raking_svy}
\alias{raking_svy}
\title{Function for running the raking algorithm from the survey package, ignoring missing marginals}
\usage{
raking_svy(
  design,
  sample.margins,
  population.margins,
  control = list(maxit = 10, epsilon = 1, verbose = FALSE),
  compress = NULL
)
}
\description{
I created this function to avoid errors when running the raking algorithm with small
samples.
}
