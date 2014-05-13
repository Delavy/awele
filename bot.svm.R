# Chargement de la biblioth?que
library (e1071)
# Chargement des donn?es
awele.data = read.table ("awele.data", sep = ",", header = T)

####################
#   Botsvm (svm)   #
####################

###VERSION 1
# Fonction de construction du modèle
svm1.create.model = function (dataset)
{
  selection = awele.data [dataset [, 14] == "G", ]
  model = svm(selection [, 1:12], selection [, 13])
  return (model)
}

# Construction du modèle
svm1.model = svm1.create.model (awele.data)


svm1.exec = function (awele, model)
{  
  g = graines.matrix (awele)
  c(typeof(g))
  g = as.data.frame (g [rep (1, 6), ])
  g = cbind (g, factor (1:6, labels = levels (awele.data [, 13])))  
  colnames (g) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""), "C") 
  return (predict (model, g, type = "raw") [, "G"])
}
# Fonction d'?valuation de la meilleure solution selon l'?tat du plateau de jeu (en utilisant la variable globale nb.model)
svm1 = function (awele) return (svm1.exec (awele, svm1.model))
