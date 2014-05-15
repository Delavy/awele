# Chargement de la biblioth?que
library (e1071)
library("fdm2id") 
# Chargement des donn?es
awele.data = read.table ("awele.data", sep = ",", header = T)


####################
#   cart1 (CART)   # PEDRO DEBUG
####################

###VERSION 1
#on prend que les gagnants
cart1.create.model = function(dataset)
{
  selection = awele.data [dataset [, 14] == "G", ]
  
  tunecontrole=tune.control(sampling="bootstrap", nboot=20, boot.size=1)
  result = splitdata(dataset = selection,target = 13:14, seed=0)  
  model = tune.rpart(result$train.x, data = result$train.y,tunecontrol=tunecontrole)$best.model
  return(model)
}
cart1.model = cart1.create.model (awele.data)

cart1.exec = function (awele, model)
{
  g = graines.matrix(awele)
  
  colnames(g) = c(paste("J",1:6,sep=""),paste("A",1:6,sep=""))
  #type = prob, conseillé par le prof
  return (max.col(predict(model,g,type="prob")))
}
cart1 = function (awele) return (cart1.exec (awele, cart1.model))

####################
#   cart2 (CART)   # ON POURRAIT OPTIMISER CART2 !
####################

###VERSION 2
# Fonction de construction du modèle
cart2.create.model = function (dataset)
{
  # On s?lectionne les instances qui correspondent aux coups jou?s par le vainqueur des affrontements
  #selection = awele.data [dataset [, 14] == "G", ]
  # Et on construit un mod?le de classification avec l'algorithme Naive bayes
  #print(dataset)
  model = rpart(R~., dataset [, 1:14], minsplit=1,cp=1)
  return (model)
}
cart2.model = cart2.create.model (awele.data)

cart2.exec = function (awele, model)
{
  g = graines.matrix (awele)
  c(typeof(g))

  g = as.data.frame (g [rep (1, 6), ])

  g = cbind (g, factor (1:6, labels = levels (awele.data [, 13])))
 
  colnames (g) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""), "C")

  var = predict(model,g,type = "prob")
 # print(var)
  return (max.col(var[,"G"]))
}
cart2 = function (awele) return (cart2.exec (awele, cart2.model))
#print (cart2)
