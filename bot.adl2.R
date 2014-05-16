# Chargement des bibliothèques
library (e1071)
library (fdm2id)
source("addData.r")

# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)

listOfFxADL = list()
listOfFxADL[[1]] = sum1ou2

########################################
# TODO : tester quelles fonctions marchent
########################################


# On sélectionne toutes les observations correspondant aux coups joués par le gagnant de la partie
# On construit un modèle d'ADL à partir des 12 premières variables de ces observations
# On essaye de prédire la 13e variable (coup joué)

# Fonction de construction du modèles
botADL2.create.model = function (dataset)
{
  dataset = addData.completeData(dataset, listOfFxADL)
  decal = addData.getDecalage(listOfFxADL)  
  
  # On sélectionne les instances qui correspondent aux coups joués par le vainqueur des affrontements
  selection = dataset [dataset [, 14+decal] == "G", ]
  # Et on construit un modèle de classification avec l'algorithme ADL
  model = LDA(selection [, 1:(12+decal)], selection [, 13+decal])
  return (model)
}
# Construction du mod?le
botADL2.model = botADL2.create.model (awele.data)
# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu et du modèle
botADL2.exec = function (awele, model)
{
  # On récupère l'état du plateau de jeu (sous la forme d'une matrice plutôt que d'un vecteur)
  g = graines.matrix (awele)
  
  g = addData.completeData(g, listOfFxADL)
  
  # On modifie les noms des colonnes pour correspondre aux noms dans l'ensemble d'apprentissage
  colnames (g)[1:12] = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  
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
botADL2 = function (awele) return (botADL2.exec (awele, botADL2.model))
