\name{dpln.knn.exec}
\alias{dpln.knn.exec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
titre
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dpln.knn.exec(awele, model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{awele}{
%%     ~~Describe \code{awele} here~~
}
  \item{model}{
%%     ~~Describe \code{model} here~~
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
function (awele, model) 
{
    g = graines.matrix(awele)
    colnames(g) = c(paste("J", 1:6, sep = ""), paste("A", 1:6, 
        sep = ""))
    prediction = predict(model, data.frame(g), type = "raw")
    ret = data.frame(matrix(data = 0, ncol = 6, nrow = 1))
    colnames(ret) = levels(prediction)
    ret[c(prediction[1])] = 1
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
