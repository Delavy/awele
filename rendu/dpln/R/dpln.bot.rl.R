####################################
#   dpln.rl( LR)
###################################

####################
### Initialisation
####################
# Chargement des bibliothèques
library (e1071)
# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)

#####################
### Création du modèle
#####################
dpln.rl.create.model = function (dataset)
{
  # On s?lectionne les instances qui correspondent aux coups jou?s par le vainqueur des affrontements
  selection = awele.data [dataset [, 14] == "G", ]
  # Et on construit un mod?le de classification avec l'algorithme Naive bayes
  model = LR(selection [, 1:12], selection [, 13])
  return (model)
}
# Construction du mod?le
dpln.rl.model = dpln.rl.create.model (awele.data)

#####################
### Execution
#####################
# Fonction d'?valuation de la meilleure solution selon l'?tat du plateau de jeu et du mod?le
dpln.rl.exec = function (awele, model)
{
  # On r?cup?re l'?tat du plateau de jeu (sous la forme d'une matrice plut?t que d'un vecteur)
  g = graines.matrix (awele)
  # On modifie les noms des colonnes pour correspondre aux noms dans l'ensemble d'apprentissage
  colnames (g) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  # On applique le mod?le et on retourne les pr?dictions (sous la forme de degr?s d'appartenance aux classes)
  prediction = predict (model, data.frame(g),type = "raw")
  
  # On crée une nouvelle dataframe pour le résultat, tout à 0, et on y colle les noms des colonnes
  ret = data.frame(matrix(data=0, ncol=6, nrow=1))
  colnames(ret) = levels(prediction)
  
  # on set la colonne retournée par prédict ) 1.
  ret[c(prediction[1])] = 1
  
  return (ret)
}
# Fonction d'?valuation de la meilleure solution selon l'?tat du plateau de jeu (en utilisant la variable globale RL.model)
dpln.rl = function (awele) return (dpln.rl.exec (awele, dpln.rl.model))