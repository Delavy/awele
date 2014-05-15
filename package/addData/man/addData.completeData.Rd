\name{addData.completeData}
\alias{addData.completeData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
La fonction complète les données
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
addData.completeData(data, listOfFx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{listOfFx}{
%%     ~~Describe \code{listOfFx} here~~
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
function (data, listOfFx) 
{
    decal = addData.getDecalage(listOfFx)
    newData = data.frame(matrix(data = 0, nr = nrow(data), nc = ncol(data) + 
        decal))
    asColNames = length(colnames(data)) > 0
    for (i in 1:12) {
        newData[, i] = data[, i]
        if (asColNames) {
            colnames(newData)[i] = colnames(data)[i]
        }
    }
    pos = 13
    for (i in 1:length(listOfFx)) {
        newData = listOfFx[[i]]@fonction(newData, pos)
        pos = pos + listOfFx[[i]]@decal
    }
    if (ncol(newData) > (12 + decal)) {
        for (i in (13 + decal):ncol(newData)) {
            newData[, i] = data[, i - decal]
            colnames(newData)[i] = colnames(data)[i - decal]
        }
    }
    return(newData)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
