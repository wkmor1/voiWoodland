\name{earth_BI}

\alias{earth_BI}
\alias{predict_BI}

\title{Model and predict Box Ironbark Woodland STM outcomes}

\description{Model and predict Box Ironbark Woodland STM outcomes using Multivariate Adaptive Regression Splines}

\usage{
earth_BI(BI_output, BI_input, n_boot, dir, verbose=FALSE)
predict_BI(newdata, dir, verbose=FALSE)
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
These functions model, and predict new data, respectively, from the output of the state and transition models for Box Ironbark Woodlands of \href{http://wkmor1.files.wordpress.com/2012/06/czembormorrisveskwintle2011.pdf}{Czembor etal 2011}. The models are fitted using Multivariate Adaptive Regression Splines (MARS) with the \code{earth} function from the package `earth'. The function \code{earth_BI} fits a set of MARS model for the selected management types specified in the \code{BI_input} list. For each management type a model is fit for each vegetation state and within in that, for each timestep from 10 through 150 years in increments of 10 years.
}

\value{
\code{earth_BI} saves its output to the specified directory.
\code{predict_BI} returns a three element list containing predictions for each management scenario. Each element of this list has itself three elements one for each state (less one state as the total cover of all four states must equal one) which in turn have 15 elements for the modelled timesteps. Each management by state by timestep prediction consists of a two element list the first of which is the full model and the second a \code{n_boot} column matrix of bootstrap prediction.
}

\references{
\href{http://wkmor1.files.wordpress.com/2012/06/czembormorrisveskwintle2011.pdf}{Czembor, C.A., Morris, W.K., Wintle, B.A. and Vesk, P.A. (2011). Quantifying variance components in ecological models based on expert opinion. Journal of Applied Ecology 48:736-745.}
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

