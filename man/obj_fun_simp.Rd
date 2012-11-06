\name{obj_fun_simp}
\alias{obj_fun_simp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
obj_fun_simp(x, sens_pp_BI, BI_output, upd.manage = c("BIW", "BIMan", "BIEco", "NONE"), k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{sens_pp_BI}{
%%     ~~Describe \code{sens_pp_BI} here~~
}
  \item{BI_output}{
%%     ~~Describe \code{BI_output} here~~
}
  \item{upd.manage}{
%%     ~~Describe \code{upd.manage} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
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
function (x, sens_pp_BI, BI_output, upd.manage = c("BIW", "BIMan", 
    "BIEco", "NONE"), k) 
{
    A <- x[1]
    B <- x[2]
    upd.manage <- match.arg(upd.manage)
    BIW <- switch(upd.manage, BIW = unlist(sens_pp_BI[[k]][["MAT"]][[15]]), 
        BIMan = unlist(subset(BI_output, Timestep == 150 & CTAbbr == 
            "BIW" & SSAbbr == "MAT", select = Area)), BIEco = unlist(subset(BI_output, 
            Timestep == 150 & CTAbbr == "BIW" & SSAbbr == "MAT", 
            select = Area)), NONE = unlist(subset(BI_output, 
            Timestep == 150 & CTAbbr == "BIW" & SSAbbr == "MAT", 
            select = Area)))
    BIMan <- switch(upd.manage, BIMan = unlist(sens_pp_BI[[k]][["MAT"]][[15]]), 
        BIW = unlist(subset(BI_output, Timestep == 150 & CTAbbr == 
            "BIMan" & SSAbbr == "MAT", select = Area)), BIEco = unlist(subset(BI_output, 
            Timestep == 150 & CTAbbr == "BIMan" & SSAbbr == "MAT", 
            select = Area)), NONE = unlist(subset(BI_output, 
            Timestep == 150 & CTAbbr == "BIMan" & SSAbbr == "MAT", 
            select = Area)))
    BIEco <- switch(upd.manage, BIEco = unlist(sens_pp_BI[[k]][["MAT"]][[15]]), 
        BIW = unlist(subset(BI_output, Timestep == 150 & CTAbbr == 
            "BIEco" & SSAbbr == "MAT", select = Area)), BIMan = unlist(subset(BI_output, 
            Timestep == 150 & CTAbbr == "BIEco" & SSAbbr == "MAT", 
            select = Area)), NONE = unlist(subset(BI_output, 
            Timestep == 150 & CTAbbr == "BIEco" & SSAbbr == "MAT", 
            select = Area)))
    mean(A * BIW + B * BIMan + (1 - A - B) * BIEco)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
