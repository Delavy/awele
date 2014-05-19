####################################
#   dpln.afd    (CDA) 
###################################

####################
### Initialisation
####################

# Chargement de la bibliothèque
library (e1071)
# Chargement des donn?es
awele.data = read.table ("awele.data", sep = ",", header = T)

#####################
### Création du modèle
#####################
dpln.afd.create.model = function (dataset)
{
  selection = awele.data [dataset [, 14] == "G", ]
  model = CDA(selection [, 1:12], selection [, 13])
  return (model)
}
# Construction du modèle
dpln.afd.model = dpln.afd.create.model (awele.data)

#####################
### Execution
#####################
dpln.afd.exec = function (awele, model)
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

dpln.afd = function (awele) return (dpln.afd.exec (awele, dpln.afd.model))


