####################################
#   dpln.cart ( CART)
#   Delaby Pierre & Nivoix Ludovic
###################################

# Chargement de la bibliothèque
library (e1071)
library("fdm2id") 
# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)


#####################
### Création du modèle
#####################
dpln.cart.create.model = function(dataset)
{
  selection = awele.data [dataset [, 14] == "G", ]
  result = splitdata(dataset = selection ,target = 13:14, seed=0)
  
  tunecontrole=tune.control(sampling="bootstrap", nboot=20, boot.size=1)  
  model = tune.rpart(C~.,cbind.data.frame(Classe=result$train.y[1], result$train.x), minsplit=1,cp=2^(-5:1), tunecontrol=tunecontrole)$best.model
  
  return(model)
}
dpln.cart.model = dpln.cart.create.model (awele.data)

#####################
### Execution
#####################
dpln.cart.exec = function (awele, model)
{
  g = graines.matrix(awele)
  
  colnames(g) = c(paste("J",1:6,sep=""),paste("A",1:6,sep=""))
  prediction = predict(model, data.frame(g),type="class")
  monretour = data.frame(matrix(data=0, ncol = 6, nrow = 1))
  colnames(monretour) = levels(prediction)
  monretour[c(prediction[1])] =1
  return (monretour)
}
dpln.cart = function (awele) return (dpln.cart.exec (awele, dpln.cart.model))
