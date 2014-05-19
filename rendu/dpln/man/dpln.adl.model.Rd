\name{dpln.adl.model}
\alias{dpln.adl.model}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(dpln.adl.model)}
\format{
  The format is:
List of 2
 $ model :List of 10
  ..$ prior  : Named num [1:6] 0.0779 0.1104 0.1883 0.1429 0.1753 ...
  .. ..- attr(*, "names")= chr [1:6] "C1" "C2" "C3" "C4" ...
  ..$ counts : Named int [1:6] 12 17 29 22 27 47
  .. ..- attr(*, "names")= chr [1:6] "C1" "C2" "C3" "C4" ...
  ..$ means  : num [1:6, 1:12] 8.92 7.41 6.69 6.36 6.48 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:6] "C1" "C2" "C3" "C4" ...
  .. .. ..$ : chr [1:12] "J1" "J2" "J3" "J4" ...
  ..$ scaling: num [1:12, 1:5] -0.0431 -0.0947 -0.1589 0.2689 0.0991 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:12] "J1" "J2" "J3" "J4" ...
  .. .. ..$ : chr [1:5] "LD1" "LD2" "LD3" "LD4" ...
  ..$ lev    : chr [1:6] "C1" "C2" "C3" "C4" ...
  ..$ svd    : num [1:5] 4.41 3.04 2.2 1.83 1.44
  ..$ N      : int 154
  ..$ call   : language lda(formula = labels ~ ., data = train)
  ..$ terms  :Classes 'terms', 'formula' length 3 labels ~ J1 + J2 + J3 + J4 + J5 + J6 + A1 + A2 + A3 + A4 + A5 + A6
  .. .. ..- attr(*, "variables")= language list(labels, J1, J2, J3, J4, J5, J6, A1, A2, A3, A4, A5, A6)
  .. .. ..- attr(*, "factors")= int [1:13, 1:12] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:13] "labels" "J1" "J2" "J3" ...
  .. .. .. .. ..$ : chr [1:12] "J1" "J2" "J3" "J4" ...
  .. .. ..- attr(*, "term.labels")= chr [1:12] "J1" "J2" "J3" "J4" ...
  .. .. ..- attr(*, "order")= int [1:12] 1 1 1 1 1 1 1 1 1 1 ...
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016690f80> 
  .. .. ..- attr(*, "predvars")= language list(labels, J1, J2, J3, J4, J5, J6, A1, A2, A3, A4, A5, A6)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:13] "factor" "numeric" "numeric" "numeric" ...
  .. .. .. ..- attr(*, "names")= chr [1:13] "labels" "J1" "J2" "J3" ...
  ..$ xlevels: Named list()
  ..- attr(*, "class")= chr "lda"
 $ method: chr "LDA"
 - attr(*, "class")= chr "model"
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
data(dpln.adl.model)
## maybe str(dpln.adl.model) ; plot(dpln.adl.model) ...
}
\keyword{datasets}
