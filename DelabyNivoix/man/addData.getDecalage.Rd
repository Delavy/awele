\name{addData.getDecalage}
\alias{addData.getDecalage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Une fonction qui calcule le décalage apporté par les classes dataSupp
}
\description{
Calcule la somme de tous les décalages apportés par toutes les fonctions dans la liste de dataSupp
}
\usage{
addData.getDecalage(listOfFx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{listOfFx}{
liste d'objet de classe dataSupp
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
retourne un entier
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (listOfFx) 
{
    decal = 0
    for (i in 1:length(listOfFx)) {
        decal = decal + listOfFx[[i]]@decal
    }
    return(decal)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
