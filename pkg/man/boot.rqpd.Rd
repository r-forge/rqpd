\name{boot.rqpd}
\alias{boot.rqpd}
\title{Bootstrapping for Panel Data Quantile Regression}
\description{
This function can be used to construct standard errors, t-statistics,
and p-values for rqpd models.
}
\usage{
boot.rqpd(ids, X, Z, y, panel, control, R = 200, bsmethod = "wxy", ...)
}
\arguments{
  \item{ids}{The ids defining the panel structure}
  \item{X}{The main part of the regression design matrix}
  \item{Z}{The augmented part of the design matrix.}
  \item{y}{The regression response vector}
  \item{panel}{The model configuration options.}
  \item{control}{Control argument for the fitting routines (see 'sfn.control').}
  \item{R}{The number of bootstrap replications}
  \item{bsmethod}{The method to be employed.  There are (as yet) only one
	option: bsmethod = "wxy" that uses the generalized bootstrap
	of Bose and Chatterjee (2003) with unit exponential weights sampled
  for each individual rather than each observation,
	see also Chamberlain and Imbens (2003).}
  \item{...}{Optional arguments to control bootstrapping}
}
\value{
  A matrix of dimension R by p is returned with the R bootstrap-estimates 
  of the vector of quantile regression parameters. 
}
\references{ 
[1] Bose, A. and S. Chatterjee, (2003) Generalized bootstrap for estimators
of minimizers of convex functions, \emph{J. Stat. Planning and Inf}, 117, 225-239.
}

\author{ Roger Koenker and Stefan Holst Bache}
\seealso{  \code{\link{summary.rqpd}}}

\keyword{ regression}
