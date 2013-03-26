\name{sens_prepost_BI}
\alias{sens_prepost_BI}
\alias{sensitivity_BI}
\title{Sensitivity analyses for Box Ironbark models}
\description{Test the sensitivity of Box Ironbark model parameters prior distributions and to updating with experiments.}
\usage{
sens_prepost_BI(dir, newdata, manage, param, n, verbose = FALSE)
sensitivity_BI(dir, newdata, manage, param, n, verbose = FALSE)
}

\arguments{
  \item{dir}{character string. Path to where Box Ironbark \code{\link{earth}} models produced by \code{\link{earth_BI}} are stored.}
  \item{newdata}{list. Input data to predict Box Ironbark output to. Must be \code{\link{BI_input}} or in the same form.}
  \item{manage}{character string. Which management strategy to test sensitivity of a parameter for. Must be one of 'BIW', 'BIMan' or 'BIEco'.}
  \item{param}{character string. Which parameters to test sensitivity for. Must be one or more of the column names of the matrices that make up \code{\link{BI_input}}.}
  \item{n}{integer. The number of values of a parameter or preposterior distributions to produce with which to test sensitivity.}
  \item{verbose}{logical. If \code{TRUE} then stage of analysis is printed to screen.}
}

\details{\code{sens_prepost_BI} makes repeated predictions based on \code{\link{earth}} model representions of the Box Ironbark state and transition model. The predictions are made with \code{n} preposterior distributions of a given parameter of the Box Ironbark state and transition model. The parameter of interest can only be one that has values that are bound by the unit interval. Each proposed preposterior distribution is a hypothetical updated distribution of the prior defined by \code{\link{BI_input}}. 
\code{sensitivity_BI} performs much the same role but instead of preposterior distributions of the parameter of interest it makes repeated predictions with the parameter fixed at even increments \code{n} times across the parameter range.}

\value{list, with three elements one for each state. Each state element has 15 elements one for each timestep. Each timestep element consists of a 2 element list one for the full earth model predictions and the second a matrix of the bootstrap sample models.}

\author{William K. Morris}

\seealso{\code{\link{earth}}, \code{\link{earth_BI}}, \code{\link{predict_BI}}, \code{\link{pre_posterior}}, \code{\link{evsi_BI}}}
\examples{
sensitivity_BI_obj <- sensitivity_BI(dir='earth_BI_objs', 
  BI_input, 'BIW', 'ProbabilityLDWtoREGgivenBIWthenCoppice',
  10, verbose=TRUE)

sens_pp_BI_obj <- sens_prepost_BI(dir='earth_BI_objs', 
  BI_input, 'BIW', 'ProbabilityLDWtoREGgivenBIWthenCoppice',
  10, verbose=TRUE)
}

