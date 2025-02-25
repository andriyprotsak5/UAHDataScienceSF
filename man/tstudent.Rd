% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/t-student_distribution.R
\name{tstudent}
\alias{tstudent}
\title{Student's t Distribution Function}
\usage{
tstudent(
  x = NULL,
  u = NULL,
  s = NULL,
  n = NULL,
  learn = FALSE,
  interactive = FALSE
)
}
\arguments{
\item{x}{Optional sample mean (not needed for interactive mode)}

\item{u}{Optional population mean (not needed for interactive mode)}

\item{s}{Optional standard deviation (not needed for interactive mode)}

\item{n}{Optional sample size (not needed for interactive mode)}

\item{learn}{Logical, if TRUE shows step-by-step explanation}

\item{interactive}{Logical, if TRUE enables interactive practice mode}
}
\value{
The t-statistic (for non-interactive mode)
}
\description{
This function calculates the t-statistic for sample data.
}
\examples{
x <- 52.9
u <- 50
s <- 3
n <- 10

# Simple calculation
tstudent(x, u, s, n)

# Learning mode
tstudent(x, u, s, n, learn = TRUE)

# Interactive mode
if(interactive()){
tstudent(interactive = TRUE)
}

}
