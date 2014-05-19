\name{dpln.svmsupp.model}
\alias{dpln.svmsupp.model}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(dpln.svmsupp.model)}
\format{
  The format is:
List of 29
 $ call           : language best.svm(x = result$train.x, y = result$train.y[, 1], gamma = 2^(-3:3), cost = 2^(-3:3), tunecontrol = tunecontrol2)
 $ type           : num 0
 $ kernel         : num 2
 $ cost           : num 2
 $ degree         : num 3
 $ gamma          : num 0.125
 $ coef0          : num 0
 $ nu             : num 0.5
 $ epsilon        : num 0.1
 $ sparse         : logi FALSE
 $ scaled         : logi [1:34] TRUE TRUE TRUE TRUE TRUE TRUE ...
 $ x.scale        :List of 2
  ..$ scaled:center: Named num [1:34] 6.78 3.85 2.65 3.74 3.64 ...
  .. ..- attr(*, "names")= chr [1:34] "J1" "J2" "J3" "J4" ...
  ..$ scaled:scale : Named num [1:34] 4.17 3.08 2.06 3.2 3.57 ...
  .. ..- attr(*, "names")= chr [1:34] "J1" "J2" "J3" "J4" ...
 $ y.scale        : NULL
 $ nclasses       : int 6
 $ levels         : chr [1:6] "C1" "C2" "C3" "C4" ...
 $ tot.nSV        : int 101
 $ nSV            : int [1:6] 25 17 20 11 19 9
 $ labels         : int [1:6] 6 4 3 2 5 1
 $ SV             : num [1:101, 1:34] -0.4261 0.0533 2.2103 -1.1451 0.2929 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:101] "279" "114" "264" "303" ...
  .. ..$ : chr [1:34] "J1" "J2" "J3" "J4" ...
 $ index          : int [1:101] 1 3 7 8 12 14 17 21 23 32 ...
 $ rho            : num [1:15] -0.1161 -0.0345 -0.3044 -0.0887 -0.4293 ...
 $ compprob       : logi FALSE
 $ probA          : NULL
 $ probB          : NULL
 $ sigma          : NULL
 $ coefs          : num [1:101, 1:5] 0.387 0.626 0.836 0.884 0.865 ...
 $ na.action      : NULL
 $ fitted         : Factor w/ 6 levels "C1","C2","C3",..: 6 4 6 4 6 3 6 6 4 2 ...
  ..- attr(*, "names")= chr [1:108] "279" "90" "114" "174" ...
 $ decision.values: num [1:108, 1:15] 1 -1 1 -1 1 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:108] "279" "90" "114" "174" ...
  .. ..$ : chr [1:15] "C6/C4" "C6/C3" "C6/C2" "C6/C5" ...
 - attr(*, "class")= chr "svm"
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
data(dpln.svmsupp.model)
## maybe str(dpln.svmsupp.model) ; plot(dpln.svmsupp.model) ...
}
\keyword{datasets}
