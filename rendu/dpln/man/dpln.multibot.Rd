\name{dpln.multibot}
\alias{dpln.multibot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
titre
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dpln.multibot(awele)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{awele}{
%%     ~~Describe \code{awele} here~~
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
function (awele) 
{
    b1 = dpln.adlkiller(awele)
    b2 = dpln.adl12(awele)
    b3 = dpln.cart(awele)
    b4 = dpln.svmsupp(awele)
    b5 = nb2(awele)
    solution = getCoupFromCoups(b1, b2, b3, b4, b5)
    return(solution)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
