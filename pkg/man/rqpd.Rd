\name{rqpd}
\alias{rqpd}
\title{Regression Quantiles for panel data (longitudinal data)}
\description{
Fit a panel data quantile regression model. The model is specified by
using an extended formula syntax (implemented with the Formula package) and
by easily configured model options (see Details).

Currently, the available models are (i) the penalized fixed-effects (FE)
estimation method proposed by Koenker (2004) and (ii) the 
correlated-random-effects (CRE) method first proposed by Abrevaya 
and Dahl (2008) and elaborated on by Bache et al (2011). 

The FE estimator is based on minimizing a weighted sum of K ordinary quantile
regression objective functions corresponding to a selection of K values of tau,
with user specified tau-specific weights.  Slope coefficients of this objective
function are tau dependent, whereas coefficients corresponding to the fixed
effects are assumed to be independent of tau.  The vector of fixed-effects
coefficients are penalized by an l1 (lasso) penalty term with associated
penalty parameter lambda, thereby shrinking these coefficients toward zero. 

The CRE estimator do not estimate the fixed effects, but controls for 
time-invariant dependence between the fixed effects and a set of covariates by
linearly including time-invariant CRE transformations of possibly
endogenous time-varying variables.
The conditional distribution of interest, is thus in some sense unconditional 
of the fixed effects.
}
\usage{
rqpd(formula, panel = panel(), data = parent.frame(),
    na.action, subset, contrasts = NULL, control = NULL, ...)
}

\arguments{
\item{formula}{
A method-specific formula for the model employing the conventions of 
the \pkg{Formula} package, which has the additional operator "|".  
"rqpd"-formulas are specified as:
\tabular{ll}{
  FE: \tab  \code{y ~ x1 + x2 + ... | s} \cr
  CRE: \tab \code{y ~ x1 + x2 + ... | s | z1 + z2 + ...} \cr
}
The portion of the formulas before the (first) vertical bar 
is specified like the conventional \code{\link{lm}} or \code{\link{rq}} 
function formulas.  

The factor variable \code{s} specifies the structure of the panel, and 
in the FE model it represents the "fixed effects". This is typically an
id column.

The last part of the CRE formula is a specification of the 
variables in the CRE component. These are possibly endogenous 
variables (in the sense that they are affected by the fixed effects)
and must be time-varying. Note that there are two vertical bars.
}

\item{panel}{
This argument specifies the panel model configurations. It is 
created (or passed directly) by the \code{panel} function. 
It constitutes a named list with options, some of which apply 
for both FE and CRE models, whereas others are method specific.
See details for available options.
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

\details{
For details on the FE model, see Koenker (2004).  
A bare bones version of this code is available from 
\url{http://www.econ.uiuc.edu/~roger/research/panel/long.html}.

The CRE models are all summarized in the paper by Bache et al. (2011).

The panel argument is created with the panel function, e.g.: 
\code{panel(taus=c(0.1, 0.25, 0.5, 0.75, 0.9), tauw=rep(1/5, 5))}.
Options not specified will get the default values. It is not
recommended to manually specify the list, as the panel function 
does some argument validation.

The available options are:
\tabular{ll}{
method (FE, CRE):\tab 
Method specificaition. "pfe" for a fixed-\cr\tab
effects estimation, "cre" for correlated-\cr\tab
random-effects estimation. Default is "pfe".\cr

taus (FE, CRE): \tab 
A vector of quantile indices in (0,1). \cr\tab
Default is 1:3/4.\cr

tauw (FE): \tab 
A vector of weights (summing to 1) for \cr\tab
the K weighted components in the FE criterion \cr\tab
function. Default is \code{c(.25, .5, .25)}.\cr

lambda (FE): \tab 
The penalty parameter controlling the \cr\tab
shrinkage of the fixed effects toward zero. \cr\tab
Default is 1.\cr

cre (CRE):\tab 
When method="cre", this is used to specify \cr\tab
the nature of the CRE component. For time-\cr\tab
means use "m" or "crem", for a specification\cr\tab
like that in Abrevaya and Dahl (2008) use \cr\tab
"ad". Default is "m", which allows for an \cr\tab
unbalanced panel. "ad" does not. \cr

ztol (FE): \tab 
A small number used to determine when \cr\tab
numerically small numbers should be \cr\tab
considered to be zero. Default is 1e-5. \cr
}

}


\value{
The function returns a fitted object representing the estimated model specified
in the formula and by the panel argument.  See 'rqpd.object' for further 
details on this object, and references to methods to look at it.
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
