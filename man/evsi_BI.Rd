\name{evsi_BI}
\alias{evsi_BI}
\alias{calc_evsi_BI}

\title{Expected value of sample information for Box Ironbark management}

\description{Calculate the expected value of sample information for Box Ironbark state and transition models}

\usage{
evsi_BI(sens_pp_BI, manage, BI_output)
calc_evsi_BI(x)
}

\arguments{
  \item{sens_pp_BI}{a Box Ironbark model parameter preposterior sensitivity analysis object}
  \item{manage}{character string, the management scenario the model parameter the value of sample information is being tested for.}
  \item{BI_output}{the original Box Ironbark state and transition model output data.}
  \item{x}{an \code{evsi_BI} object for which to calculate the expected value of sample information}
}

\details{\code{evsi_BI} takes the output of a \code{\link{sens_prepost}} derived preposterior sensitivity analysis for a given Box Ironbark model parameter and calculates the components of the expected value of information for that parameter using \code{\link{constrOptim}} to maximise the objective function defined by \code{\link{obj_fun_simp}}. Those components being the expected value of each individual preposterior distribution and the expected value given the prior distribution. \code{calc_evsi_BI} takes the output of \code{evsi_BI} and calculates EVSI as the mean of the expected value for individual preposterior distributions minus the the expected value given the prior distribution.}

\value{\code{evsi_BI} returns a two element list. The first element is the expected value given the prior distribution (no new information) and the second is a list of expected values for each preposterior distribution produced by \code{\link{sens_prepost}}. 
\code{calc_evsi_BI} returns the expected value of sample information as length 1 numeric vector.}

\references{href{http://books.google.com/books?id=tJE_AQAAIAAJ}{Raiffa, H. and Schlaifer, R. (1968). Applied statistical decision theory. John Wiley & Sons Inc., New York.}}

\author{William K. Morris.}

\seealso{\code{\link{constrOptim}}, \code{\link{sens_propost}}, \code{\link{obj_fun_simp}}}

\examples{
load('sens_pp_ProbabilityLDWtoREGgivenBIWthenCoppice.rda')
evsi_BI_obj <- evsi_BI(sens_pp_BI=sens_pp_BI, manage='BIW', BI_output=BI_output)
calc_evsi_BI(evsi_BI_obj)
}

