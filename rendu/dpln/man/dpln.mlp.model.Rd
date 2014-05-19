\name{dpln.mlp.model}
\alias{dpln.mlp.model}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(dpln.mlp.model)}
\format{
  The format is:
List of 19
 $ n            : int [1:3] 12 6 6
 $ nunits       : int 25
 $ nconn        : num [1:26] 0 0 0 0 0 0 0 0 0 0 ...
 $ conn         : num [1:120] 0 1 2 3 4 5 6 7 8 9 ...
 $ nsunits      : int 19
 $ decay        : num 0.1
 $ entropy      : logi FALSE
 $ softmax      : logi TRUE
 $ censored     : logi FALSE
 $ value        : num 147
 $ wts          : num [1:120] -0.491 -1.195 1.048 0.78 -1.923 ...
 $ convergence  : int 1
 $ fitted.values: num [1:154, 1:6] 0.0114 0.0596 0.0227 0.011 0.0254 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:154] "1" "2" "3" "4" ...
  .. ..$ : chr [1:6] "C1" "C2" "C3" "C4" ...
 $ residuals    : num [1:154, 1:6] -0.0114 -0.0596 -0.0227 -0.011 -0.0254 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:154] "1" "2" "3" "4" ...
  .. ..$ : chr [1:6] "C1" "C2" "C3" "C4" ...
 $ lev          : chr [1:6] "C1" "C2" "C3" "C4" ...
 $ call         : language best.nnet(x = C ~ ., data = cbind.data.frame(Classe = selection[13], selection[1:12]), size = 2:6, decay = c(0.1, 0.01, 0.001), tunecontrol = tunecontrole)
 $ terms        :Classes 'terms', 'formula' length 3 C ~ J1 + J2 + J3 + J4 + J5 + J6 + A1 + A2 + A3 + A4 + A5 + A6
  .. ..- attr(*, "variables")= language list(C, J1, J2, J3, J4, J5, J6, A1, A2, A3, A4, A5, A6)
  .. ..- attr(*, "factors")= int [1:13, 1:12] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:13] "C" "J1" "J2" "J3" ...
  .. .. .. ..$ : chr [1:12] "J1" "J2" "J3" "J4" ...
  .. ..- attr(*, "term.labels")= chr [1:12] "J1" "J2" "J3" "J4" ...
  .. ..- attr(*, "order")= int [1:12] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..- attr(*, "intercept")= int 1
  .. ..- attr(*, "response")= int 1
  .. ..- attr(*, ".Environment")=<environment: 0x00000000174f5518> 
  .. ..- attr(*, "predvars")= language list(C, J1, J2, J3, J4, J5, J6, A1, A2, A3, A4, A5, A6)
  .. ..- attr(*, "dataClasses")= Named chr [1:13] "factor" "numeric" "numeric" "numeric" ...
  .. .. ..- attr(*, "names")= chr [1:13] "C" "J1" "J2" "J3" ...
 $ coefnames    : chr [1:12] "J1" "J2" "J3" "J4" ...
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
data(dpln.mlp.model)
## maybe str(dpln.mlp.model) ; plot(dpln.mlp.model) ...
}
\keyword{datasets}
