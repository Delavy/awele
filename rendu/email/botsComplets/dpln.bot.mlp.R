####################################
#   dpln.mlp (NNET)
#   Delaby Pierre & Nivoix Ludovic
###################################

####################
### Initialisation
####################

# Chargement des bibliothèques
library (e1071)
library("fdm2id") 

# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)


#####################
### Création du modèle
#####################
dpln.mlp.create.model = function(dataset)
{
  
  selection = awele.data [dataset [, 14] == "G", ]
  tunecontrole = tune.control(sampling="bootstrap",nboot=20,boot.size=1)
  model = tune.nnet(C~., data = cbind.data.frame(Classe = selection[13], selection[1:12] ),size = 2:6,decay= c(.1,.01,.001),tunecontrol = tunecontrole)$best.model
  
  return (model) 
}
dpln.mlp.model =dpln.mlp.create.model (awele.data)

#####################
### Execution
#####################
dpln.mlp.exec = function (awele, model)
{ 
  g = graines.matrix (awele)
  colnames (g) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  
  prediction = factor(predict (model, as.data.frame(g), type = "class"))

  ret = data.frame(matrix(data=0, ncol=6, nrow=1))
  colnames(ret) = levels(prediction)
  
  return (ret)

}
dpln.mlp = function (awele) return (dpln.mlp.exec (awele, dpln.mlp.model))

