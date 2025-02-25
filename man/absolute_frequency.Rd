% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/absolute_frequency.R
\name{absolute_frequency}
\alias{absolute_frequency}
\title{Absolute Frequency Function}
\usage{
absolute_frequency(v = NULL, x = NULL, learn = FALSE, interactive = FALSE)
}
\arguments{
\item{v}{Optional numeric vector (not needed for interactive mode)}

\item{x}{Optional numeric value to count (not needed for interactive mode)}

\item{learn}{Logical, if TRUE shows step-by-step explanation}

\item{interactive}{Logical, if TRUE enables interactive practice mode}
}
\value{
The absolute frequency of x in v (for non-interactive mode)
}
\description{
This function calculates the absolute frequency of a value in a numeric vector.
}
\examples{
data <- c(1,4,3,3,2,5,7,12,1,2,3,12)
value <- 12

# Simple calculation
absolute_frequency(data, value)

# Learning mode
absolute_frequency(data, value, learn = TRUE)

# Interactive mode
if(interactive()){
absolute_frequency(interactive = TRUE)
}

}
