\name{dpln.mlp.create.model}
\alias{dpln.mlp.create.model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
titre
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dpln.mlp.create.model(dataset)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
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
function (dataset) 
{
    selection = awele.data[dataset[, 14] == "G", ]
    tunecontrole = tune.control(sampling = "bootstrap", nboot = 20, 
        boot.size = 1)
    model = tune.nnet(C ~ ., data = cbind.data.frame(Classe = selection[13], 
        selection[1:12]), size = 2:6, decay = c(0.1, 0.01, 0.001), 
        tunecontrol = tunecontrole)$best.model
    return(model)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
