####################################
#   dpln.knn (KNN)
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
dpln.knn.create.model = function (dataset)
{
  tunecontrol2=tune.control(sampling="bootstrap", nboot=20, boot.size=1) 
  selection = awele.data [dataset [, 14] == "G", ]  
  result = splitdata(dataset = selection,target = 13:14, seed=0)
  voisins_knn = tune.knn(result$train.x,result$train.y[,2], k=1:10, tunecontrol=tunecontrol2)
  model = KNN(train= result$train.x, labels=result$train.y[,1],k=1)  
  return (model)
}
# Construction du modèle
dpln.knn.model = dpln.knn.create.model (awele.data)

#####################
### Execution
#####################
dpln.knn.exec = function (awele, model)
{
  g = graines.matrix (awele)
  colnames (g) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  
  prediction = predict (model, data.frame(g),type = "raw")
  
  # On crée une nouvelle dataframe pour le résultat, tout à 0, et on y colle les noms des colonnes
  ret = data.frame(matrix(data=0, ncol=6, nrow=1))
  colnames(ret) = levels(prediction)
  
  # on set la colonne retournée par prédict à 1.
  ret[c(prediction[1])] = 1
  return (ret)
}
dpln.knn = function (awele) return (dpln.knn.exec (awele, dpln.knn.model))
