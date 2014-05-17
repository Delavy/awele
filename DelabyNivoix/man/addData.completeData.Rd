\name{addData.completeData}
\alias{addData.completeData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function qui rajoute les données au tableau existant
}
\description{
La fonction execute les fonctions passées en paramètres pour ajouter des données aux données de l'awalé qui lui sont passées
}
\usage{
addData.completeData(data, listOfFx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Les données de l'awélé
}
  \item{listOfFx}{
Une liste de fonction de la classe dataSupp
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Retourne les données complétées
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Delaby Pierre et Nivoix Ludovic
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
addData.completeData(awele.data,list(sum1ou2))

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
