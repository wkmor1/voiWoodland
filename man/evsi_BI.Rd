\name{evsi_BI}
\alias{evsi_BI}
\alias{evpi_BI}
\alias{calc_evi_BI}

\title{Expected value of information for Box Ironbark management}

\description{Calculate the expected value of sample or perfect information for Box Ironbark state and transition models}

\usage{
evsi_BI(BIW=NULL, BIMan=NULL, BIEco=NULL, BI_output)
evpi_BI(BI_output, n)
calc_evi_BI(x)
}

\arguments{
  \item{BIW}{a Box Ironbark model parameter preposterior sensitivity analysis object for the \code{BIW} management scenario.}
  \item{BIMan}{a Box Ironbark model parameter preposterior sensitivity analysis object for the \code{BIMan} management scenario.}
  \item{BIEco}{a Box Ironbark model parameter preposterior sensitivity analysis object for the \code{BIEco} management scenario.}
  \item{BI_output}{the original Box Ironbark state and transition model output data.}
  \item{x}{an \code{evsi_BI} or \code{evpi_BI} object for which to calculate the expected value of information.}
  \item{n}{integer, number of alternate model parameterisations to use to calculate EVPI from.}
}

\details{\code{evsi_BI} takes the outputs of \code{\link{sens_prepost_BI}} derived preposterior sensitivity analyeis for Box Ironbark model parameters and calculates the components of the expected value of information for that parameter using \code{\link{constrOptim}} to maximise the objective function defined by \code{\link{obj_fun_simp}}. Those components being, the expected value of each individual preposterior distribution and the expected value given the prior distribution. \code{evpi_BI} calcualates the equivalent components for the value of perfect information. \code{calc_evi_BI} takes the output of \code{evsi_BI} or \code{evpi_BI} and calculates the EVI as the mean of the expected value for individual preposterior distributions (or the alternate model parameterisations for EVPI) minus the the expected value given the prior distribution.}

\value{\code{evsi_BI}/\code{evpi_BI} returns a two element list. The first element is the expected value given the prior distribution (no new information) and the second is a list of expected values for each preposterior distribution  produced by \code{\link{sens_prepost} or alternate model parameterisation of \code{BI_output}}. 
\code{calc_evi_BI} returns the expected value of information as length 1 numeric vector.}

\references{\href{http://books.google.com/books?id=tJE_AQAAIAAJ}{Raiffa, H. and Schlaifer, R. (1968). Applied statistical decision theory. John Wiley & Sons Inc., New York.}}

\author{William K. Morris.}

\seealso{\code{\link{constrOptim}}, \code{\link{sens_prepost_BI}}, \code{\link{obj_fun_simp}}}

\examples{
load('sens_pp_ProbabilityLDWtoREGgivenBIWthenCoppice.rda')
evsi_BI_obj <- evsi_BI(BIW=sens_pp_BI, manage='BIW', BI_output=BI_output)
calc_evsi_BI(evsi_BI_obj)
}

