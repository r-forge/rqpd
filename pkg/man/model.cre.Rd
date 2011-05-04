\name{model.cre}
\alias{model.cre}
\title{
  Generate CRE variables.
}
\description{
  This function generates CRE variables of type 'CRE-M' or 'AD' from  
  variables available in a model frame.
  This function is not intended for direct use but is called from the 
  function rqpd.}

\usage{
  model.cre(object, data, ids, contrasts.arg, method)
}

\arguments{
  \item{object}{A model formula or a \code{terms} object.}
  \item{data}{a data frame created with \code{model.frame}.}
  \item{ids}{An id vector (potentially as factor) specifying the 
    structure of the panel.}
  \item{contrasts.arg}{A list, whose entries are values (numeric matrices or
          character strings naming functions) to be used as replacement
          values for the \code{contrasts} replacement function and whose
          names are the names of columns of \code{data} containing
          'factor's.}
\item{method}{"m"/"crem" for CRE-M specification or "ad" for the
  Abrevaya and Dahl type specification.}
}

\value{Matrix with the generated CRE variables.}
\author{
  Stefan Holst Bache
}

\keyword{internal}
\keyword{Quantile regression}
\keyword{Correlated-random-effects}
