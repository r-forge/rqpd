\name{bwd}
\docType{data}
\alias{bwd}
\title{A subset of birthweight data used by Abrevaya (2006).}
\description{
  This dataset contains a subset of the birthweight data used by
  Abrevaya (2006). It has observations from Arizona and washington, 
  and only some variables are included. The entire (subset) of this 
  data is available in JAE's online data archive.

\tabular{ll}{
  momid3     \tab  identification number of the mother                       \cr
  idx        \tab  index number of a mother's birth (starts with 1 and goes  \cr
             \tab  to at least 2 for each mother); momid3 and idx together   \cr
             \tab  describe the panel structure of the data                  \cr
  stateres   \tab  code (1 through 51) for state of residence; see the NCHS  \cr
             \tab  (National Center for Health Statistics) website for codes \cr
  dmage      \tab  age of mother (in years)                                  \cr
  dmeduc     \tab  education of mother (in years)                            \cr
  dbirwt     \tab  birthweight (in grams)                                    \cr
  smoke      \tab  indicator variable for smoking status (1=smoker,          \cr
             \tab  0=nonsmoker)                                              \cr
  male       \tab  indicator variable for baby gender (1=male, 0=female)     \cr
  year       \tab  year of birth (0=1990, ..., 8=1998)                       \cr
  married    \tab  indicator variable for marital status (1=married,         \cr
 		         \tab  0=unmarried)                                              \cr
  agesq      \tab  age of mother squared                                     \cr
  black      \tab  indicator variable for black race (1=black, 0=white)      \cr
  novisit    \tab  indicator that no prenatal visit occurred                 \cr
  pretri2    \tab  indicator that first prenatal visit occurred in 2nd       \cr
 		         \tab  trimester                                                 \cr
  pretri3    \tab  indicator that first prenatal visit occurred in 3nd       \cr
 		         \tab  trimester                                                 \cr
}
}
\usage{data(bwd)}
\format{A dataframe with 13154 observations and 14 variables}
\source{http://econ.queensu.ca/jae/2006-v21.4/abrevaya/}
\references{
[1]  Abrevaya, J. (2006) \emph{Estimating the effect of smoking on birth outcomes using a matched panel data approach}. Journal of Applied Econometrics, 21-4, pages 489--519.

[2]  http://econ.queensu.ca/jae/2006-v21.4/abrevaya/
}
\keyword{datasets}
