\name{panel}
\alias{panel}
\title{Model configurations for panel methods in rqpd}
\description{
This function creates a configuration list for use with \code{rqpd}.
For unspecified options defaults will be used.
}
\usage{
  panel(method="pfe", taus=1:3/4, tauw=c(.25,.5,.25), 
    lambda=1, cre="m", ztol=1e-5)
}

\arguments{
\item{method}{
Either "pfe" for the penalized fixed-effects model by Koenker (2004) or "cre"
for a correlated-random-effects model, see Abrevaya and Dahl (2008) and 
Bache et al. (2011).
}

\item{taus}{
Used when \code{method="pfe"}. 
A K-vector of tau's lying in (0,1).  For the pfe model, the coefficients of the
covariate effects before the bar are allowed to vary with these tau's, whereas
the fixed effect coefficients on the model specification after the bar are
assumed to be independent of tau. For a CRE specification all coefficients
vary with these taus.
}
\item{tauw}{
Used when \code{method="pfe"}. 
Weights for each component of the objective function which determine the 
impact for each tau used for estimation.
}
\item{lambda}{
The (non-negative) penalty parameter used in the \code{pfe} method to 
shrink fixed-effects toward zero.
}
\item{cre}{
Used if \code{method="cre"} with value "m" og "ad". 
If a CRE model is specified, this determines the 
type of CRE variables included in the model. When "m" is used (default),
then time-means of the relevant variables are included, and when "ad" is
used then the specification by Abrevaya and Dahl (2008) is used.
This latter option requires a balanced panel.
}
\item{ztol}{
Used when \code{method="pfe"}. 
A parameter controlling the tolerance determining how close estimated
coefficients need to be to zero to be considered zero.
}
}

\value{
  A named list with configuration options for \code{rqpd}.
}

\seealso{
\code{\link{rqpd}}.
}
\examples{
  p <- panel(taus=1:5/4, tauw=rep(1/5, 5))
  # fit <- rqpd(formula, p, data=data)
}
\keyword{regression}
\keyword{quantile regression}
\keyword{panel data}
\keyword{robust}
