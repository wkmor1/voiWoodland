\name{obj_fun_simp}
\alias{obj_fun_simp}
\alias{obj_fun_parks}

\title{Objective function for Box Ironbark management}
\description{A simple objective function for decision analysis of Box Ironbark forest and woodland management.}
\usage{
obj_fun_simp(x, BI_output, BIW=NULL, BIMan=NULL, BIEco=NULL, k)
obj_fun_parks(x, BI_output, BIW=NULL, BIEco=NULL, k)
}

\arguments{
  \item{x}{numeric vector with two elements. The first element is the proportion of the landscape to undergo no management while the second is the proportion remaining under current day management. These values must be positive and their must be equal to or less than one.}
  \item{BI_output}{The output data of the Box Ironbark state and transition models}
  \item{BIW}{logical}
  \item{BIMan}{logical}
  \item{BIEco}{logical}
  \item{k}{numeric. Which of prepostior distributions}
}

\details{This objective function calculates the value of assigning management strategies proportionly to the landscape. The expected value of the outcome is measured in terms of the proportion of the landscape that ends up in the 'Mature' state after 150 years. If all all managment options are \code{NULL} then \code{BI_output} is used in the objective value calculation for all management strategies. }
\value{numeric. The proportion of the landscape in the 'Mature' state.}
\author{William K. Morris}

\seealso{\code{\link{sens_prepost_BI}}, \code{\link{evsi_BI}}}
\examples{
data(BI_output)
obj_fun_simp(c(1/3, 1/3), BI_output, k=10)
}

