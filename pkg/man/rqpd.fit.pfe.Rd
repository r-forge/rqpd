\name{rqpd.fit.pfe}
\alias{rqpd.fit.pfe}
\title{Fitting routine for pfe panel data quantile regression.}
\description{
This is the fitting routine used by \code{rqpd} and \code{rqpd.boot}.
}
\usage{
rqpd.fit.pfe(X, Z, y, taus, tauw, lambda, control, ...)
}

\arguments{
\item{X}{Main part of the design matrix in csr format}
\item{Z}{The augmented "panel-part" of the design matrix in csr format.}
\item{y}{The response vector (non-sparse).}
\item{taus}{ A K-vector of tau's lying in (0,1)}
\item{tauw}{ A K-vector of weights summing to 1.}
\item{lambda}{ Penalty parameter controlling the amount of shrinkage on 
  the fixed effects towards zero.}
\item{control}{Control argument for the fitting routines (see 'sfn.control').}
\item{\dots}{Additional arguments passed to solver.}
}

\value{
The function returns a list with coefficients and some 
information from the solver on convergence.
}

\author{
Roger Koenker and Stefan Bache

Maintainer: Stefan Bache <rqpd@stefanbache.dk>
}

\seealso{
\code{\link{rqpd}, \link{summary.rqpd}}, \code{\link{rqpd.object}}.
}

\keyword{regression}
\keyword{quantile regression}
\keyword{panel data}
\keyword{robust}
