# Chargement de la biblioth?que
library (e1071)
# Chargement des donn?es
awele.data = read.table ("awele.data", sep = ",", header = T)

####################
#   BotAFD (CDA)   #
####################

###VERSION 1
# Fonction de construction du modèle
afd1.create.model = function (dataset)
{
  selection = awele.data [dataset [, 14] == "G", ]
  model = CDA(selection [, 1:12], selection [, 13])
  return (model)
}

# Construction du modèle
afd1.model = afd1.create.model (awele.data)

# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu et du modèle
afd1.exec = function (awele, model)
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

afd1 = function (awele) return (afd1.exec (awele, afd1.model))


