\name{rqpd.object}
\alias{rqpd.object}

\title{
Panel Data Quantile Regression Object 
}
\description{
  These are objects of class \code{"rqpd"}.
  They represent the fit of a panel data quantile function model. 
}
\section{Generation}{
  This class of objects is returned from the \code{rqpd} function 
  to represent a fitted panel data quantile regression model. 
}
\section{Methods}{
  The \code{"rqpd"} class of objects has methods for the following generic 
functions: 
\code{coef}
, \code{print}
, \code{print.summary}
, \code{summary}.
}
\section{Structure}{
  The following components must be included in a legitimate \code{rqpd} object. 
  \describe{
    \item{\code{coefficients}}{
      The coefficients of the quantile regression fit. 
      The names of the coefficients are the names of the 
      single-degree-of-freedom effects (the columns of the 
      model matrix) augmented by the quantile index, tau. 
    }
    \item{\code{residuals}}{
      the residuals from the fit.
    }
    \item{\code{contrasts}}{
      a list containing sufficient information to construct the contrasts 
      used to fit any factors occurring in the model. 
      The list contains entries that are either matrices or character vectors. 
      When a factor is coded by contrasts, the corresponding contrast matrix 
      is stored in this list. 
      Factors that appear only as dummy variables and variables in the model 
      that are matrices correspond to character vectors in the list. 
      The character vector has the level names for a factor or the column 
      labels for a matrix. 
    }
    \item{\code{X}}{
      The model matrix in csr format.
    }
    \item{\code{Z}}{
      Either an incidence matrix with fixed-effects indicators,
      or CRE variables. Stored in csr format.
    }
    \item{\code{y}}{
      The response vector.
    }
    \item{\code{taus}}{
      The vector of quantile indices.
    }
    \item{\code{panel}}{
      The list with model configurations.
    }
    \item{\code{control}}{
      Control argument for the fitting routines (see 'sfn.control')
    }
    \item{\code{ids}}{
      Vector of ids defining the panel structure.
    }
    \item{\code{formula}}{
      The formula object given to rqpd.
    }
    \item{\code{call}}{
      The rqpd call.
    }
    \item{\code{ierr}}{
      Error code reported by \code{rq.fit.sfn}.
    }
    \item{\code{it}}{
      Number of iterations used by the solver \code{rq.fit.sfn}.
    }
  }
}
\details{
  The coefficients and residuals may be extracted 
  by the generic functions of the same name, rather than 
  by the \code{$} operator.
  }
\seealso{
  \code{\link{rqpd}},  \code{\link{coefficients}}, \code{\link{summary}}.   
}
\keyword{regression}
\keyword{quantile regression}
