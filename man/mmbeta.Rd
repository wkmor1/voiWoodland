\name{mmbeta}

\alias{mmbeta}

\title{Fit a beta distribution via moment matching.}

\description{Estimate the shape parameters of a beta distributions via moment matching of a sample dataset}

\usage{mmbeta(x)}

\arguments{
 \item{x}{numeric vector. All elements must be positive and less than one.}
}

\details{This function is used to obtain point estimates of the shape parameters of a beta distribution using matching of its first two moments.}

\value{numeric, a length two vector of the two shape parameters of a beta distribution.}

\author{
William K. Morris.
}

\seealso{\code{link{dbeta}}}

\examples{
x <- rbeta(100, .5, .5)
mmbeta(x)
}
