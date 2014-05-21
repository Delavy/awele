####################################
#   dpln.mlpsupp ( MLP + dataSupp)
#   Delaby Pierre & Nivoix Ludovic
###################################

####################
### Initialisation
####################

# Chargement de la bibliothèque
library (e1071)
library("fdm2id") 
source("dpln.addData.R")

# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)

# On crée la liste des objets dataSupp
listMLP = list(sum1ou2);

#####################
### Création du modèle
#####################
dpln.mlpsupp.create.model = function(dataset)
{
  #On ajoute les données
  dataset = addData.completeData(dataset, listMLP)
  # On calcule le décalage
  decal = addData.getDecalage(listMLP)
  
  selection = dataset [dataset [, 14+decal] == "G", ]
  tunecontrole = tune.control(sampling="bootstrap",nboot=20,boot.size=1)
  model = tune.nnet(C~., data = cbind.data.frame(Classe = selection[13+decal], selection[1:(12+decal)] ),size = 2:(6+decal),decay= c(.1,.01,.001),tunecontrol = tunecontrole)$best.model
  
  return (model) 
}
dpln.mlpsupp.model =dpln.mlpsupp.create.model (awele.data)

#####################
### Execution
#####################
dpln.mlpsupp.exec = function (awele, model)
{ 
  g = graines.matrix (awele)
  g = addData.completeData(g, listMLP)
  
  colnames(g)[1:12] = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  
  prediction = factor(predict (model, as.data.frame(g), type = "class"))
  
  ret = data.frame(matrix(data=0, ncol=6, nrow=1))
  colnames(ret) = c (paste ("C", 1:6, sep = ""))
  
  resultat = ret[c(prediction[1])] = 1
  
  return (ret)
  
}
dpln.mlpsupp = function (awele) return (dpln.mlpsupp.exec (awele, dpln.mlpsupp.model))
