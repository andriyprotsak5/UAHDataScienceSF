% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/harmonic_mean.R
\name{harmonic_mean}
\alias{harmonic_mean}
\title{Harmonic Mean Function}
\usage{
harmonic_mean(x = NULL, learn = FALSE, interactive = FALSE)
}
\arguments{
\item{x}{Optional numeric vector (not needed for interactive mode)}

\item{learn}{Logical, if TRUE shows step-by-step explanation}

\item{interactive}{Logical, if TRUE enables interactive practice mode}
}
\value{
The harmonic mean of the vector (for non-interactive mode)
}
\description{
This function calculates the harmonic mean of a numbers vector.
}
\examples{
data <- c(1,4,3,3,2,5,7,12,1,2,3,12)

# Simple calculation
harmonic_mean(data)

# Learning mode
harmonic_mean(data, learn = TRUE)

# Interactive mode
if(interactive()){
harmonic_mean(interactive = TRUE)
}

}
