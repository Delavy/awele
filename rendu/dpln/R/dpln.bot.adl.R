####################################
#   dpln.adl (LDA)
###################################

####################
### Initialisation
####################

# Chargement des bibliothèques
library (e1071)
library (fdm2id)
# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)

######################
### Création du modèle
######################
# Fonction de construction du modèles
dpln.adl.create.model = function (dataset)
{
  # On sélectionne les instances qui correspondent aux coups joués par le vainqueur des affrontements
  selection = awele.data [dataset [, 14] == "G", ]
  # Et on construit un modèle de classification avec l'algorithme ADL
  model = LDA(selection [, 1:12], selection [, 13])
  return (model)
}
# Construction du mod?le
dpln.adl.model = dpln.adl.create.model (awele.data)
# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu et du modèle
dpln.adl.exec = function (awele, model)
{
  # On récupère l'état du plateau de jeu (sous la forme d'une matrice plutôt que d'un vecteur)
  g = graines.matrix (awele)
  # On modifie les noms des colonnes pour correspondre aux noms dans l'ensemble d'apprentissage
  colnames (g) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  # On applique le modèle et on retourne les pr?dictions (sous la forme de degrés d'appartenance aux classes)
  prediction = predict (model, data.frame(g),type = "raw")
  
  # On crée une nouvelle dataframe pour le résultat, tout à 0, et on y colle les noms des colonnes
  ret = data.frame(matrix(data=0, ncol=6, nrow=1))
  colnames(ret) = levels(prediction)
  
  # on set la colonne retournée par prédict ) 1.
  ret[c(prediction[1])] = 1
  
  return (ret)
}
# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu (en utilisant la variable globale nb.model)
dpln.adl = function (awele) return (dpln.adl.exec (awele, dpln.adl.model))