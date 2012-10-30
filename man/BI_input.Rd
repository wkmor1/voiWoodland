\name{BI_input}
\alias{BI_input}
\docType{data}
\title{Input data for Box Ironbark Forest and Woodland state and transition models.}
\description{
  The \code{BI_input} list provides three matrices one for each management scenario.
  The columns of each matrix correspond to samples from the expert elicited parameter distributions.
}
\usage{data(BI_input)}
\format{
  List, each of 3 elements is a matrix of parameters with 375 samples from the joint prior distribution.
  \describe{
    \item{\code{BIW}}{Set of parameters for models under the `natural disturbance only' management scenario.}
    \item{\code{BIMan}}{Set of parameters for models under the `current/status quo' management scenario.}
    \item{\code{BIEco}}{Set of parameters for models under the `ecological thinning' management scenario.}
  }
}
\details{
 The \code{BI_input} data are described in Czembor etal (2011).

\source{

}
\references{}
\examples{
data(BI_input)
str(BI_input)
}
\keyword{datasets}
