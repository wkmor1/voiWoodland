\name{earth_BI}

\alias{earth_BI}
\alias{predict_BI}

\title{Model and predict Box Ironbark Woodland STM outcomes}

\description{Model and predict Box Ironbark Woodland STM outcomes using Multivariate Adaptive Regression Splines}

\usage{
earth_BI(n_boot, BIModelOutput, inputs)
predict(x, newdata=NULL)
}

\arguments{
 \item{n_boot}{integer. number of bootstrap samples to fit in addition to full model.}
 \item{BIModelOutput}{}
 \item{inputs}{}
 \item{x}{a model object produced by \code{earth_BI}.}
 \item{newdata}{list of 3 matrices of input data to predict area from. List elements must have names coresponding to management scenarios.}
 \item{verbose}{}
}

\details{
These functions model, and predict new data, respectively, from the output of the state and transition models for Box Ironbark Woodlands of Czembor etal 2011 (\link{http://wkmor1.files.wordpress.com/2012/06/czembormorrisveskwintle2011.pdf}). The models are fitted using Multivariate Adaptive Regression Splines (MARS) with the \code{earth} function from the package `earth'. The function earth_BI fits a set of MARS model for the selected management types specified in the \code{inputs} list. For each management a models are fit for each vegetation state. 
}

\value{

}

\references{

}

\author{
William K. Morris.
}

\seealso{
\code{\link{earth}}, \code{\link{BIModelOutput}}, \code{\link{BIWParams}}, \code{\link{BIManParams}} and \code{\link{BIEcoParams}}
}

\examples{
data(BIModelOutput)
data(BIWParams)
data(BIManParams)
data(BIEcoParams)

inputs <- list(BIW=BIWParams, BIMan=BIManParams, BIEco=BIEcoParams)
    
earth_BI_obj <- earth_BI(n_boot=9, BIModelOutput=BIModelOutput, 
  inputs=inputs)
      
earth_BI_preds <- predict_BI(earth_BI_obj, inputs)    
}

