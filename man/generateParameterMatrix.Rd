% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_gems.R
\name{generateParameterMatrix}
\alias{generateParameterMatrix}
\title{generate a template for mean parameters}
\usage{
generateParameterMatrix(hf,
  statesNamesFrom = (dimnames(hf@list.matrix)["from"])[[1]],
  statesNamesTo = statesNamesFrom)
}
\arguments{
\item{hf}{a \code{transition.structure} of dimension \eqn{N \times N}{N x
N}, where \eqn{N} is the number of states.}

\item{statesNamesFrom}{- a list of names of states in the model. By default list(State 1, State 2, ..., State N)}

\item{statesNamesTo}{- a list of names of states in the model. By default the same as statesNamesFrom}
}
\value{
a \code{transition.structure} of dimension \eqn{N \times N}{N x N},
whose components are \code{lists} of the right length for the parameters in
the corresponding hazard function \code{hf}.
}
\description{
This function simplifies generating the matrix of mean parameters from a
matrix of transition functions.
}
\seealso{
\code{\link{transition.structure-class}},
\code{\link{simulateCohort}}
}
\author{
Luisa Salazar Vizcaya, Nello Blaser, Thomas Gsponer
}
\keyword{utilities}
