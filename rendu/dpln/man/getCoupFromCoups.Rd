\name{getCoupFromCoups}
\alias{getCoupFromCoups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
titre
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getCoupFromCoups(b1, b2, b3, b4, b5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b1}{
%%     ~~Describe \code{b1} here~~
}
  \item{b2}{
%%     ~~Describe \code{b2} here~~
}
  \item{b3}{
%%     ~~Describe \code{b3} here~~
}
  \item{b4}{
%%     ~~Describe \code{b4} here~~
}
  \item{b5}{
%%     ~~Describe \code{b5} here~~
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
function (b1, b2, b3, b4, b5) 
{
    somme = coup = matrix(0, ncol = 6, nrow = 1)
    somme = as.matrix(b1 + b2 + b3 + b4 + b5)
    valMax = max(somme)
    nbMax = sum(somme == valMax)
    if (nbMax > 1) {
        b5null = sum(b5 == 0) == 6
        b4null = sum(b4 == 0) == 6
        if (b5null || b4null) {
            coup = b1
        }
        else {
            vecNull = matrix(0, ncol = 6, nrow = 1)
            coup = getCoupFromCoups(b1, b2, b3, vecNull, vecNull)
        }
    }
    else {
        coup[somme == valMax] = 1
        coup[somme != valMax] = 0
    }
    return(as.data.frame(coup))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
