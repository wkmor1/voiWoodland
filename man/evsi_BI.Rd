\name{evsi_BI}
\alias{evsi_BI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
evsi_BI(sens_pp_BI, manage, BI_output)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sens_pp_BI}{
%%     ~~Describe \code{sens_pp_BI} here~~
}
  \item{manage}{
%%     ~~Describe \code{manage} here~~
}
  \item{BI_output}{
%%     ~~Describe \code{BI_output} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (sens_pp_BI, manage, BI_output) 
{
    init <- c(1/3, 1/3)
    ui <- rbind(c(1, 0), c(0, 1), c(-1, 0), c(0, -1), c(-1, -1))
    ci <- c(0, 0, -1, -1, -1)
    e_max <- constrOptim(init, function(x) {
        obj_fun_simp(x, BI_output = BI_output, upd.manage = "NONE")
    }, NULL, ui, ci, control = list(fnscale = -1))
    max_k <- mclapply(1:10, function(k) {
        constrOptim(init, function(x) {
            obj_fun_simp(x, sens_pp_BI = sens_pp_BI, BI_output = BI_output, 
                upd.manage = manage, k)
        }, NULL, ui, ci, control = list(fnscale = -1))
    }, mc.cores = detectCores())
    return(list(e_max, max_k))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
