\name{rqpd.fit.cre}
\alias{rqpd.fit.cre}
\title{Fitting routine fori CRE panel data quantile regression.}
\description{
This is the fitting routine used by \code{rqpd} and \code{rqpd.boot}.
}
\usage{
rqpd.fit.cre(X, Z, y, taus, control, ...)
}

\arguments{
\item{X}{Main part of the design matrix in csr format}
\item{Z}{The augmented "panel-part" of the design matrix in csr format.}
\item{y}{The response vector (non-sparse).}
\item{taus}{ A K-vector of tau's lying in (0,1)}
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
