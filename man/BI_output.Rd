\name{BI_ouput}
\alias{BI_output}
\docType{data}
\title{Output data for Box Ironbark Forest and Woodland state and transition models.}
\description{The \code{BI_output} dataset provides the predicted proportions of Box Ironbark Woodland and Forests in different states given three different management scenarios.
}

\usage{BI_output}
\format{
  A dataset with of 506250 observations of 5 parameters: 
  \describe{
    \item{\code{Project}}{The parameter set, of 375 posssible sets, prediction is derived from. Corresponds to row names of \code{\link{BI_input}} matrices.}
    \item{\code{Timestep}}{Timestep prediction is for.}
    \item{\code{CTAbbr}}{Management scenario for prediction. \code{BIW} = no mangement; \code{BIEco} = ecological thinning; \code{BIMan} = current management maintained.}
    \item{\code{SSAbbr}}{The state the cover value represents. \code{HDW} = high density woodland; \code{LDW} = low density woodland; \code{MAT} = mature woodland.}
    \item{\code{Area}}{Proportion of area in the state at given timestep under given management scenario.}
  }
}

\details{The \code{BI_output} data are described in Czembor etal (2011). The state \code{REG} = regenerating woodland, has been excluded from the dataset as the cover of all states must at add up to one making having all states present in the dataset redundant. For each of the 375 \code{Project}'s there are ten replicates of each state by timestep by management scenario combination.}

\source{\href{http://wkmor1.files.wordpress.com/2012/06/czembormorrisveskwintle2011.pdf}{Czembor, C.A., Morris, W.K., Wintle, B.A. and Vesk, P.A. (2011). Quantifying variance components in ecological models based on expert opinion. Journal of Applied Ecology 48:736-745.}
}

\examples{
data(BI_output)
str(BI_output)
}

\keyword{datasets}
