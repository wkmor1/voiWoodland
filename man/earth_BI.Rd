\name{earth_BI}

\alias{earth_BI}
\alias{predict_BI}

\title{Model and predict Box Ironbark Woodland STM outcomes}

\description{Model and predict Box Ironbark Woodland STM outcomes using Multivariate Adaptive Regression Splines}

\usage{
earth_BI(BI_output, BI_input, n_boot, dir, verbose=FALSE)
predict(newdata, dir, verbose=FALSE)
}

\arguments{
 \item{BI_output}{data frame. The output from the BIFAW management state and transition model.}
 \item{BI_input}{list. Input parameters elicted from experts for BIFAW management state and transition models}
 \item{n_boot}{integer. Number of bootstrap samples to fit in addition to full model.}
 \item{dir}{character string. Path to where the MARS submodel objects will be stored.}
 \item{newdata}{list. In the same format as `inputs'. List elements are matrices and must have names coresponding to management scenarios with colum names corresponding to the parameter names of `inputs'.}
 \item{verbose}{logical. If \code{TRUE} then the model being fit (or predicted) is displayed on the console.}
}

\details{
These functions model, and predict new data, respectively, from the output of the state and transition models for Box Ironbark Woodlands of Czembor etal 2011 (\link{http://wkmor1.files.wordpress.com/2012/06/czembormorrisveskwintle2011.pdf}). The models are fitted using Multivariate Adaptive Regression Splines (MARS) with the \code{earth} function from the package `earth'. The function earth_BI fits a set of MARS model for the selected management types specified in the \code{inputs} list. For each management type a model is fit for each vegetation state and within in that, for each timestep 10 through 150 years in increments of 10 years. 
}

\value{

}

\references{

}

\author{
William K. Morris.
}

\seealso{
\code{\link{earth}}, \code{\link{BI_output}}, \code{\link{BI_input}}
}

\examples{
data(BI_output)
data(BI_input)

earth_BI(BI_output=BI_output, BI_input=BI_input,
  n_boot=9, dir=tempdir(), verbose=TRUE)

earth_BI_preds <- predict_BI(BI_input, dir=tempdir(), verbose=TRUE)   
}

