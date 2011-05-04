\name{rqpd}
\alias{rqpd}
\title{Quantile Regression for Longitudinal (Panel) Data}
\description{This function implements the penalized fixed effect (FE)
estimation method proposed in Koenker (2004) and the correlated-random-effects
(CRE) method proposed by Abrevaya and Dahl (2008) and elaborated on by Bache et
al (2011). 

The FE estimator is based on minimizing a weighted sum of K ordinary quantile
regression objective functions corresponding to a selection of K values of tau,
with user specified tau-specific weights.  Slope coefficients of this objective
function are tau dependent, whereas coefficients corresponding to the fixed
effects are assumed to be independent of tau.  The vector of fixed effect
coefficients are penalized by an l1 (lasso) penalty term with associated
penalty parameter lambda, thereby shrinking these coefficients toward zero. 

The CRE estimator do not estimate the fixed effects, but controls for 
time-invariant dependence between the fixed effects and a set of covariates by
linearly including time-invariant CRE transformations of the latter.
The fixed-effects are, as such, considered part of the "error term". 
}
\usage{
rqpd(formula, panel = panel(), data = parent.frame(),
    na.action, subset, contrasts = NULL, control = NULL, ...)
}

\arguments{
\item{formula}{
A formula for the model employing the conventions of the \pkg{Formula} package.
The portion of the formula before the vertical bar is specified like the
conventional \code{\link{lm}} or \code{\link{rq}} functions.  The portion of
the formula after the vertical bar specifies the structure of the panel and 
whether it is a FE or CRE model. The latter component would typically be a 
single factor variable indicating the identities of the sample "individuals" 
for a FE specification, or a specification of the CRE term.
factor structure for the fixed effects is possible for estimation, but not
(yet) for inference.
}


\item{panel}{A named list with model configuration options. For the
FE model one can specify \code{lambda}, the penalty parameter controlling
the shrinkage of the fixed effects toward zero, and 
\code{tauw}, a K-vector of positive weights, summing to one, specifying 
the relative weight to be given to the K distinct components of the 
objective function. For the CRE specification, one can specify \code{cre}
to be either "m" for time means, or "ad" for the "Chamberlain"-like 
specification used by Abrevaya and Dahl (2008). The latter requires a 
balanced panel.
}

\item{data}{
A data.frame containing the variables as specified in the formula.
}

\item{na.action}{
A function to filter missing data.  This is applied to the model.frame after
any subset argument has been used.  The default (with 'na.fail') is to create
an error if any missing values are found.  A possible alternative is 'na.omit',
which deletes observations that contain one or more missing values.
}
\item{subset}{
An optional vector specifying a subset of observations to be
          used in the fitting process.
}

\item{contrasts}{
A list giving contrasts for some or all of the factors default = 'NULL'
appearing in the model formula.  The elements of the list should have the same
name as the variable and should be either a contrast matrix (specifically, any
full-rank matrix with as many rows as there are levels in the factor), or else
a function to compute such a matrix given the number of levels.
}
\item{control}{Control argument for the fitting routines (see 'sfn.control').} 
\item{\dots}{ Other arguments passed to fitting routines.  }
}

\details{For details on the FE model, see Koenker (2004).  
A bare bones version of this code is available from 
\url{http://www.econ.uiuc.edu/~roger/research/panel/long.html}.
The CRE models are all summarized in the paper by Bache et al. (2011).
}

\value{
The function returns a fitted object representing the estimated model specified
in the formula.  See 'rqss.object' for further details on this object, and
references to methods to look at it.
}
\references{
[1] Abrevaya, Jason and Christian M. Dahl. 2008. \emph{The effects of 
birth inputs on birthweight.} Jounal of Business and Economic Statistics. 
26-4. Pages 379--397.

[2] Bache, Stefan Holst; Christian M. Dahl; Johannes Tang Kristensen. 2011. 
\emph{Headlights on tobacco road to low birthweight--Evidence from a battery
of quantile regression estimators and a heterogeneous panel.}

[3] Koenker, Roger. 2004. \emph{Quantile regression for longitudinal data}. 
Journal of Multivariate Analysis. 91-1. Pages 74--89.
}
\author{
Roger Koenker and Stefan Bache

Maintainer: Stefan Bache <rqpd@stefanbache.dk>
}

\seealso{
\code{\link{summary.rqpd}}, \code{\link{rqpd.object}}.
}
\examples{
set.seed(10)
m <- 3
n <- 10
s <- as.factor(rep(1:n,rep(m,n)))
x <- exp(rnorm(n*m))
u <- x*rnorm(m*n) + (1-x)*rf(m*n,3,3)
a <- rep(rnorm(n),rep(m,n))
y <- a + u
fit <- rqpd(y ~ x | s, panel(lambda = 5))
sfit <- summary(fit)

# A CRE model
data(bwd)

cre.form <- dbirwt ~ smoke + dmage + agesq + 
   novisit + pretri2 + pretri3 | momid3 | smoke + 
   dmage + agesq 

# CRE-M type fit:
crem.fit <- rqpd(cre.form, panel(method="cre"), data=bwd)

# AD type fit:
ad.fit <- rqpd(cre.form, panel(method="cre", cre="ad"), data=bwd,
  subset=bwd$idx \%in\% 1:2)
}
\keyword{regression}
\keyword{quantile regression}
\keyword{panel data}
\keyword{robust}
