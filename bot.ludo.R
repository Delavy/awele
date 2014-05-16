# Chargement de la biblioth?que
library (e1071)
library("fdm2id") 
# Chargement des donn?es
awele.data = read.table ("awele.data", sep = ",", header = T)


####################
#   mlpludo (nnet)    # 
####################

###VERSION 1
#on prend que les gagnants
mlpludo.create.model = function(dataset)
{
  
  selection = awele.data [dataset [, 14] == "G", ]
  tunecontrole = tune.control(sampling="bootstrap",nboot=20,boot.size=1)
  model = tune.nnet(C~., data = cbind.data.frame(Classe = selection[13], selection[1:12] ),size = 2:6,decay= c(.1,.01,.001),tunecontrol = tunecontrole)$best.model
  
  return (model) 
}
mlpludo.model =mlpludo.create.model (awele.data)

mlpludo.exec = function (awele, model)
{ 
  g = graines.matrix (awele)
  colnames (g) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  
  prediction = factor(predict (model, as.data.frame(g), type = "class"))

  ret = data.frame(matrix(data=0, ncol=6, nrow=1))
  colnames(ret) = levels(prediction)
  
  #resultat = ret[c(prediction[1])] = 1
  
  return (ret)

}
mlpludo = function (awele) return (mlpludo.exec (awele, mlpludo.model))

