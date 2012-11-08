\name{pre_posterior}

\alias{pre_posterior}

\title{Update a prior distribution.}

\description{Create a set of preposterior distributions by updating a unit interval prior distribution with hypothetical binomial trials.}

\usage{pre_posterior(x, n, size)}

\arguments{
 \item{x}{numeric vector. A sample from the prior distribution to update.}
 \item{n}{integer. The number of preposterior distributions to produce.}
 \item{size}{integer. The number of binomial trials to conduct for each experiment}
}

\details{
A binomial event, A, may occur with probability, p. But there is a > 0, belief, p', that p = 0. The prior belief in p can therefore be described by a zero inflated beta distribution where if p = 0 then:p ~ Bernoulii(1-p') and if 0 < p =< 1 then p ~ Beta(s1, s2). We can do an experiment, e, such that we perform, n, trials to observe m occurances of, A, to update our belief in p. If m > 0 then we update the belief in p by assigning p' = 0 and the posterior distribution of p becomes: p ~ Beta(s1 + m, s2 + (n - m)). If m = 0 then we update the belief in p by retaining the prior value of p'. The posterior distribution of p then remains a zero inflated beta distribution where if p = 0 then: p ~ Bernoulii(1-p') and if 0 < p =< 1 then p ~ Beta(s1, s2 + n)}

\value{list. Each element is a preposterior distribution of x with the same length as x.}

\author{
William K. Morris.
}

\seealso{\code{\link{mmbeta}}}

\examples{
x <- rbeta{100, .5, .5}
mean(x)
x.pp <- pre_posterior(x, 10, 1000)
str(x.pp)
lapply(x.pp, mean)
}
