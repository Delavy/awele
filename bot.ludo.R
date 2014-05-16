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
  #print(selection)
  model = tune.nnet(C~., data = cbind.data.frame(selection[1:12], Classe = selection[13]),size = 2:6,tunecontrol = tunecontrole)$best.model
  #print(model)
  
  return (model) 
}
mlpludo.model =mlpludo.create.model (awele.data)

mlpludo.exec = function (awele, model)
{ 
  g = graines.matrix (awele)
  colnames (g) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  
  prediction = predict (model, as.data.frame(g), type = "class")
  #print(prediction)
  
 # ret = data.frame(matrix(data=0, ncol=6, nrow=1))
  #colnames(ret) = levels(prediction)
  
  #ret[c(prediction[1])] = 1
  return (prediction)

}
mlpludo = function (awele) return (mlpludo.exec (awele, mlpludo.model))

