####################################
#   cartsupp (CART)   
###################################


####################
### Initialisation
####################
# Chargement de la biblioth?que
library (e1071)
library("fdm2id") 
source("dpln.addData.R")

# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)

# On crée la liste des objets dataSupp
listCart = list(sum1ou2, somme, vide);

######
# Contre cart 1 : 
# sum1ou2, vide, posMax, bidoua : cart 1
# sum1ou2 : dpln.cartsupp ( 26 // 23)
# vide : egalite
# posMax : egalite
# bidoua : cart1
#nbVidesAvantPleine : egalite
# somme : dpln.cartsupp
# unTour : egalite
# sum1ou2, somme : dpln.cartsupp


#####################
### Création du modèle
#####################
dpln.cartsupp.create.model = function(dataset)
{
  #On ajoute les données
  dataset = addData.completeData(dataset, listCart)
  # On calcule le décalage
  decal = addData.getDecalage(listCart)
  
  selection = dataset [dataset [, 14+decal] == "G", ]  
  result = splitdata(dataset = selection ,target = (13+decal):(14+decal), seed=0)
  
  tunecontrole=tune.control(sampling="bootstrap", nboot=20, boot.size=1)  
  
  model = tune.rpart(C~.,cbind.data.frame(Classe=result$train.y[1], result$train.x), minsplit=1,cp=2^(-5:1), tunecontrol=tunecontrole)$best.model
  
  return(model)
}
dpln.cartsupp.model = dpln.cartsupp.create.model (awele.data)

#####################
### Execution
#####################
dpln.cartsupp.exec = function (awele, model)
{
  g = graines.matrix(awele)
  g = addData.completeData(g, listCart)
  colnames(g)[1:12] = c(paste("J",1:6,sep=""),paste("A",1:6,sep=""))
  #type = prob, conseillé par le prof
  prediction = predict(model, data.frame(g),type="class")
  monretour = data.frame(matrix(data=0, ncol = 6, nrow = 1))
  colnames(monretour) = levels(prediction)
  monretour[c(prediction[1])] =1
  return (monretour)
}
dpln.cartsupp = function (awele) return (dpln.cartsupp.exec (awele, dpln.cartsupp.model))
