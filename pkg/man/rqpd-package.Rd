\name{rqpd-package}
\alias{rqpd-package}
\docType{package}
\title{
  Regression quantiles for panel data (longitudinal data)
}
\description{
  The \code{rqpd} package provides quantile regression 
  estimation routines and bootstrap inference for panel 
  (longitudinal) data.
  Currently, the available estimation methods are the 
  penalized fixed-effects model (Koenker, 2004) and a 
  correlated-random-effects type model.
  (Abrevaya and Dahl, 2008; Bache et al. 2011).}
\details{
\tabular{ll}{%
Package: \tab rqpd\cr%
Type: \tab Package\cr
Version: \tab 0.5\cr
Date: \tab 2011-04-26\cr
License: \tab GPL >= 2.0 \cr
LazyLoad: \tab yes\cr
LazyData:\tab yes\cr
}
}

\author{
Roger Koenker and Stefan Holst Bache

Maintainer: Stefan Holst Bache <pdqr@stefanbache.dk>
}

\references{
[1] Abrevaya, Jason and Christian M. Dahl. 2008. \emph{The effects of 
birth inputs on birthweight.} Journal of Business and Economic Statistics. 
26-4. Pages 379--397.

[2] Bache, Stefan Holst; Christian M. Dahl; Johannes Tang Kristensen. 2011. 
\emph{Headlights on tobacco road to low birthweight--Evidence from a battery
of quantile regression estimators and a heterogeneous panel.}

[3] Koenker, Roger. 2004. \emph{Quantile regression for longitudinal data}. 
Journal of Multivariate Analysis. 91-1. Pages 74--89.
}
\examples{
data(bwd)

### A fixed-effects model:

# formula is specified as
fe.form <- dbirwt ~ smoke + dmage + agesq + 
   novisit + pretri2 + pretri3 | as.factor(momid3) 

# estimate the model:
fe.fit <- rqpd(fe.form, panel(lambda=0.5), data=bwd)

### A CRE model:

# formula is specified as
cre.form <- dbirwt ~ smoke + dmage + agesq + 
   novisit + pretri2 + pretri3 | momid3 | smoke + 
   dmage + agesq

# estimate the model:
cre.fit <- rqpd(cre.form, panel(method="cre"), data=bwd)
}
\keyword{package}
