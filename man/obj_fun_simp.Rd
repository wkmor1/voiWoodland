\name{obj_fun_simp}
\alias{obj_fun_simp}

\title{Objective function for Box Ironbark management}
\description{A simple objective function for decision analysis of Box Ironbark forest and woodland management.}
\usage{obj_fun_simp(x, sens_pp_BI, BI_output, upd.manage = c("BIW", "BIMan", "BIEco", "NONE"), k)}

\arguments{
  \item{x}{numeric vector with two elements. The first element is the proportion of the landscape to undergo no management while the second is the proportion remaining under current day management. These values must be positive and their must be equal to or less than one.}
  \item{sens_pp_BI}{a preposterior sensitivity analysis object produced by \code{link{sens_prepost_BI}}. May be omitted and is ignored if \code{upd.manage='NONE'}}.
  \item{BI_output}{The output data of the Box Ironbark state and transition models}
  \item{upd.manage}{character string. Which management strategy does the parameter for which the preposterior outcome is being analysed refer to. If 'NONE' the \code{BI_output} is used in the objective value calculation for all management strategies.}
  \item{k}{numeric. Which of prepostior distributions to use from the \code{sens_pp_BI} object. May be omitted and is ignored if \code{upd.manage='NONE'}}
}

\details{This objective function calculates the value of assigning management strategies proportionly to the landscape. The expected value of the outcome is measured in terms of the proportion of the landscape that ends up in the 'Mature' state after 150 years. }
\value{numeric. The proportion of the landscape in the 'Mature' state.}
\author{William K. Morris}

\seealso{\code{link{sens_prepost_BI}}, \code{\link{evsi_BI.r}}}
\examples{
obj_fun_simp(c(1/3, 1/3), BI_output, "BIW")
}

