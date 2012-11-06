\name{sens_prepost_BI}
\alias{sens_prepost_BI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sens_prepost_BI(dir, newdata, manage, param, n, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{
%%     ~~Describe \code{dir} here~~
}
  \item{newdata}{
%%     ~~Describe \code{newdata} here~~
}
  \item{manage}{
%%     ~~Describe \code{manage} here~~
}
  \item{param}{
%%     ~~Describe \code{param} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function (dir, newdata, manage, param, n, verbose = FALSE) 
{
    sensitivity_BI_obj <- vector("list", n)
    gc(FALSE)
    preposts <- pre_posterior(newdata[[manage]][, param], n, 
        1000)
    for (i in seq_len(n)) {
        inputs <- newdata
        inputs[[manage]][, param] <- preposts[[i]]
        predict_BI_obj <- vector("list", 3)
        gc(FALSE)
        names(predict_BI_obj) <- c("LDW", "HDW", "MAT")
        for (j in names(predict_BI_obj)) {
            predict_BI_obj_j <- vector("list", 15)
            gc(FALSE)
            for (k in seq_along(predict_BI_obj_j)) {
                predict_BI_obj_jk <- vector("list", 2)
                gc(FALSE)
                if (verbose) 
                  cat("Prediction: ")
                load(sprintf("\%s/\%s_\%s_\%s.rda", dir, manage, 
                  j, k))
                predict_BI_obj_jk[[1]] <- as.integer(((plogis(predict(earth_BI_obj_ijk[[1]], 
                  inputs[[manage]])) - 0.001)/0.998))
                gc(FALSE)
                if (verbose) 
                  cat(sprintf("\%s \%s \%s \%s \%s \n", param, i, 
                    manage, j, k))
                predict_BI_obj_jk[[2]] <- sapply(seq_along(earth_BI_obj_ijk[[2]]), 
                  function(REP) {
                    as.integer(((plogis(predict(earth_BI_obj_ijk[[2]][[REP]], 
                      inputs[[manage]])) - 0.001)/0.998))
                  })
                gc(FALSE)
                rm(earth_BI_obj_ijk)
                predict_BI_obj_j[[k]] <- predict_BI_obj_jk
                gc(FALSE)
            }
            predict_BI_obj[[j]] <- predict_BI_obj_j
            gc(FALSE)
        }
        sensitivity_BI_obj[[i]] <- predict_BI_obj
    }
    attr(sensitivity_BI_obj, "tested_values") <- preposts
    return(sensitivity_BI_obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
