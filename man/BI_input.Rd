\name{BI_input}
\alias{BI_input}
\docType{data}
\title{Input data for Box Ironbark Forest and Woodland state and transition models.}
\description{
  The \code{BI_input} list provides three matrices one for each management scenario.
  The columns of each matrix correspond to samples from the expert elicited parameter distributions.
}

\usage{BI_input}

\format{
  List, each of 3 elements is a matrix of parameters with 375 samples from the joint prior distribution.
  \describe{
    \item{\code{BIW}}{Set of parameters for models under the `natural disturbance only' management scenario.}
    \item{\code{BIMan}}{Set of parameters for models under the `current/status quo' management scenario.}
    \item{\code{BIEco}}{Set of parameters for models under the `ecological thinning' management scenario.}
  }
}

\details{
 The \code{BI_input} data are described in Czembor etal (2011). The dataset consists of a list of three matrices. Each matrix is a sample from the joint prior distribution for the expert elicited models of Box Ironbark Forest and Woodland state and transition models under a different management scenario. The parameters that make up each joint prior have 375 estimates derived by sampling randomly from three plausible distributions 25 times given three point elicitions from five experts.}

\source{\href{http://wkmor1.files.wordpress.com/2012/06/czembormorrisveskwintle2011.pdf}{Czembor, C.A., Morris, W.K., Wintle, B.A. and Vesk, P.A. (2011). Quantifying variance components in ecological models based on expert opinion. Journal of Applied Ecology 48:736-745.}}

\examples{
data(BI_input)
str(BI_input)
}

\keyword{datasets}
