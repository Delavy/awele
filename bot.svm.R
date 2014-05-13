# Chargement de la biblioth?que
library (e1071)
library("fdm2id") 
# Chargement des donn?es
awele.data = read.table ("awele.data", sep = ",", header = T)

####################
#   Botsvm (svm)   #
####################

###VERSION 1
# Fonction de construction du modèle
svm1.create.model = function (dataset)
{
  tunecontrol2=tune.control(sampling="bootstrap", nboot=20, boot.size=1)    
  selection = awele.data [dataset [, 14] == "G", ]  
  result = splitdata(dataset = selection,target = 13:14, seed=0)  
  model = tune.svm(result$train.x,result$train.y[,1], gamma=2^(-3:3), cost=2^(-3:3), tunecontrol=tunecontrol2)$best.model  
  return (model)
}

# Construction du modèle
svm1.model = svm1.create.model (awele.data)


svm1.exec = function (awele, model)
{  
  g = graines.matrix (awele)
  colnames (g) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  prediction = predict (model, data.frame(g),type = "raw")
  
  # On crée une nouvelle dataframe pour le résultat, tout à 0, et on y colle les noms des colonnes
  ret = data.frame(matrix(data=0, ncol=6, nrow=1))
  colnames(ret) = levels(prediction)
  
  # on set la colonne retournée par prédict ) 1.
  ret[c(prediction[1])] = 1
  return (ret)
}
# Fonction d'?valuation de la meilleure solution selon l'?tat du plateau de jeu (en utilisant la variable globale nb.model)
svm1 = function (awele) return (svm1.exec (awele, svm1.model))
