\name{dpln.mlpsupp.model}
\alias{dpln.mlpsupp.model}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(dpln.mlpsupp.model)}
\format{
  The format is:
List of 19
 $ n            : int [1:3] 14 6 6
 $ nunits       : int 27
 $ nconn        : num [1:28] 0 0 0 0 0 0 0 0 0 0 ...
 $ conn         : num [1:132] 0 1 2 3 4 5 6 7 8 9 ...
 $ nsunits      : int 21
 $ decay        : num 0.1
 $ entropy      : logi FALSE
 $ softmax      : logi TRUE
 $ censored     : logi FALSE
 $ value        : num 143
 $ wts          : num [1:132] 0.32 0.66 0.015 -2.517 2.257 ...
 $ convergence  : int 1
 $ fitted.values: num [1:154, 1:6] 0.013549 0.006237 0.000364 0.0087 0.012018 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:154] "1" "2" "3" "4" ...
  .. ..$ : chr [1:6] "C1" "C2" "C3" "C4" ...
 $ residuals    : num [1:154, 1:6] -0.013549 -0.006237 -0.000364 -0.0087 -0.012018 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:154] "1" "2" "3" "4" ...
  .. ..$ : chr [1:6] "C1" "C2" "C3" "C4" ...
 $ lev          : chr [1:6] "C1" "C2" "C3" "C4" ...
 $ call         : language best.nnet(x = C ~ ., data = cbind.data.frame(Classe = selection[13 + decal], selection[1:(12 + decal)]), size = 2:(6 + decal), decay = c(0.1,      0.01, 0.001), tunecontrol = tunecontrole)
 $ terms        :Classes 'terms', 'formula' length 3 C ~ J1 + J2 + J3 + J4 + J5 + J6 + A1 + A2 + A3 + A4 + A5 + A6 + unoudeuxJ + unoudeuxA
  .. ..- attr(*, "variables")= language list(C, J1, J2, J3, J4, J5, J6, A1, A2, A3, A4, A5, A6, unoudeuxJ, unoudeuxA)
  .. ..- attr(*, "factors")= int [1:15, 1:14] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:15] "C" "J1" "J2" "J3" ...
  .. .. .. ..$ : chr [1:14] "J1" "J2" "J3" "J4" ...
  .. ..- attr(*, "term.labels")= chr [1:14] "J1" "J2" "J3" "J4" ...
  .. ..- attr(*, "order")= int [1:14] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..- attr(*, "intercept")= int 1
  .. ..- attr(*, "response")= int 1
  .. ..- attr(*, ".Environment")=<environment: 0x00000000145bc138> 
  .. ..- attr(*, "predvars")= language list(C, J1, J2, J3, J4, J5, J6, A1, A2, A3, A4, A5, A6, unoudeuxJ, unoudeuxA)
  .. ..- attr(*, "dataClasses")= Named chr [1:15] "factor" "numeric" "numeric" "numeric" ...
  .. .. ..- attr(*, "names")= chr [1:15] "C" "J1" "J2" "J3" ...
 $ coefnames    : chr [1:14] "J1" "J2" "J3" "J4" ...
 $ xlevels      : Named list()
 - attr(*, "class")= chr [1:2] "nnet.formula" "nnet"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(dpln.mlpsupp.model)
## maybe str(dpln.mlpsupp.model) ; plot(dpln.mlpsupp.model) ...
}
\keyword{datasets}
