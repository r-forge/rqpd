\name{summary.rqpd}
\alias{summary.rqpd}
\title{
Summary method for Panel Data Quantile Regression (rqpd).
}
\description{
Returns a summary list for a panel data quantile regression fit.
}
\usage{
\method{summary}{rqpd}(object, se = "boot", covariance = FALSE, ...)
}
\arguments{
  \item{object}{
    This is an object of class \code{rqpd}  produced by 
    a call to \code{rqpd()}.
  }
  \item{se}{
    Specifies the method used to compute standard standard errors.  There
    is currently only one method:
    \enumerate{
      \item \code{"boot"} which implements a weighted x-y bootstrap,
      see \code{\link{boot.rqpd}}.
    }
  }
  \item{covariance}{
    logical flag to indicate whether the full covariance matrix of the 
    estimated parameters should be returned. 
  }
  \item{...}{
    Optional arguments to summary, e.g. \code{R} for number of bootstrapping.       iterations, see \code{\link{boot.rq}}
   }
}
\value{
  a list is returned with the following components.

\item{coefficients}{
  a p by 4 matrix consisting of the coefficients, their estimated standard
  errors, their t-statistics, and their associated p-values.
}
\item{cov}{
  the estimated covariance matrix for the coefficients in the model,
  provided that \code{cov=TRUE} in the called sequence.
}
}

\seealso{
  \code{\link{rqpd}}
  \code{\link{boot.rqpd}}
}

\keyword{regression}
