% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standard_deviation.R
\name{standard_deviation}
\alias{standard_deviation}
\title{Standard Deviation Function}
\usage{
standard_deviation(x = NULL, learn = FALSE, interactive = FALSE)
}
\arguments{
\item{x}{Optional numeric vector (not needed for interactive mode)}

\item{learn}{Logical, if TRUE shows step-by-step explanation}

\item{interactive}{Logical, if TRUE enables interactive practice mode}
}
\value{
The standard deviation of the vector (for non-interactive mode)
}
\description{
This function calculates the standard deviation of a numbers vector.
}
\examples{
data <- c(1,5,3,7,10,4,2)

# Simple calculation
standard_deviation(data)

# Learning mode
standard_deviation(data, learn = TRUE)

# Interactive mode
if(interactive()){
standard_deviation(interactive = TRUE)
}

}
