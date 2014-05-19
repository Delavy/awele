\name{dpln.cartsupp.model}
\alias{dpln.cartsupp.model}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(dpln.cartsupp.model)}
\format{
  The format is:
List of 14
 $ frame              :'data.frame':	3 obs. of  9 variables:
  ..$ var       : Factor w/ 2 levels "<leaf>","J6": 2 1 1
  ..$ n         : int [1:3] 108 33 75
  ..$ wt        : num [1:3] 108 33 75
  ..$ dev       : num [1:3] 80 19 47
  ..$ yval      : num [1:3] 6 3 6
  ..$ complexity: num [1:3] 0.175 0.0375 0
  ..$ ncompete  : int [1:3] 4 0 0
  ..$ nsurrogate: int [1:3] 5 0 0
  ..$ yval2     : num [1:3, 1:14] 6 3 6 9 1 8 12 4 8 23 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : NULL
  .. .. ..$ : chr [1:14] "" "" "" "" ...
 $ where              : Named int [1:108] 3 3 3 3 3 2 3 3 3 2 ...
  ..- attr(*, "names")= chr [1:108] "279" "90" "114" "174" ...
 $ call               : language best.rpart(formula = C ~ ., data = cbind.data.frame(Classe = result$train.y[1], result$train.x), minsplit = 1, cp = 2^(-5:1), tunecontrol = tunecontrole)
 $ terms              :Classes 'terms', 'formula' length 3 C ~ J1 + J2 + J3 + J4 + J5 + J6 + A1 + A2 + A3 + A4 + A5 + A6 + unoudeuxJ + unoudeuxA + sumJ + sumA + videsJ + videsA
  .. ..- attr(*, "variables")= language list(C, J1, J2, J3, J4, J5, J6, A1, A2, A3, A4, A5, A6, unoudeuxJ, unoudeuxA, sumJ, sumA, videsJ, videsA)
  .. ..- attr(*, "factors")= int [1:19, 1:18] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:19] "C" "J1" "J2" "J3" ...
  .. .. .. ..$ : chr [1:18] "J1" "J2" "J3" "J4" ...
  .. ..- attr(*, "term.labels")= chr [1:18] "J1" "J2" "J3" "J4" ...
  .. ..- attr(*, "order")= int [1:18] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..- attr(*, "intercept")= int 1
  .. ..- attr(*, "response")= int 1
  .. ..- attr(*, ".Environment")=<environment: 0x0000000014335950> 
  .. ..- attr(*, "predvars")= language list(C, J1, J2, J3, J4, J5, J6, A1, A2, A3, A4, A5, A6, unoudeuxJ, unoudeuxA, sumJ, sumA, videsJ, videsA)
  .. ..- attr(*, "dataClasses")= Named chr [1:19] "factor" "numeric" "numeric" "numeric" ...
  .. .. ..- attr(*, "names")= chr [1:19] "C" "J1" "J2" "J3" ...
 $ cptable            : num [1:2, 1:5] 0.175 0.0625 0 1 1 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:2] "1" "2"
  .. ..$ : chr [1:5] "CP" "nsplit" "rel error" "xerror" ...
 $ method             : chr "class"
 $ parms              :List of 3
  ..$ prior: num [1:6(1d)] 0.0833 0.1111 0.213 0.1574 0.1759 ...
  .. ..- attr(*, "dimnames")=List of 1
  .. .. ..$ : chr [1:6] "1" "2" "3" "4" ...
  ..$ loss : num [1:6, 1:6] 0 1 1 1 1 1 1 0 1 1 ...
  ..$ split: num 1
 $ control            :List of 9
  ..$ minsplit      : num 1
  ..$ minbucket     : num 0
  ..$ cp            : num 0.0625
  ..$ maxcompete    : num 4
  ..$ maxsurrogate  : num 5
  ..$ usesurrogate  : num 2
  ..$ surrogatestyle: num 0
  ..$ maxdepth      : num 30
  ..$ xval          : num 10
 $ functions          :List of 3
  ..$ summary:function (yval, dev, wt, ylevel, digits)  
  ..$ print  :function (yval, ylevel, digits)  
  ..$ text   :function (yval, dev, wt, ylevel, digits, n, use.n)  
 $ numresp            : int 8
 $ splits             : num [1:10, 1:5] 108 108 108 108 108 0 0 0 0 0 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:10] "J6" "videsJ" "videsA" "A3" ...
  .. ..$ : chr [1:5] "count" "ncat" "improve" "index" ...
 $ variable.importance: Named num [1:6] 5.692 1.035 0.862 0.69 0.69 ...
  ..- attr(*, "names")= chr [1:6] "J6" "videsJ" "A1" "A3" ...
 $ y                  : int [1:108] 6 4 6 4 6 3 6 6 4 2 ...
 $ ordered            : Named logi [1:18] FALSE FALSE FALSE FALSE FALSE FALSE ...
  ..- attr(*, "names")= chr [1:18] "J1" "J2" "J3" "J4" ...
 - attr(*, "xlevels")= Named list()
 - attr(*, "ylevels")= chr [1:6] "C1" "C2" "C3" "C4" ...
 - attr(*, "class")= chr "rpart"
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
data(dpln.cartsupp.model)
## maybe str(dpln.cartsupp.model) ; plot(dpln.cartsupp.model) ...
}
\keyword{datasets}
