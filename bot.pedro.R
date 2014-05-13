# Chargement de la bibliothèque
library (e1071)

# Chargement des fonction de addData
source("addData.r")


######### Preparation des objets d'enrichissement de données
### Initialisation de la liste contenant les objets d'enrichisement
listOfFx = list()
listOfFx[[1]] = sum1ou2
#pedro.fx[[2]] = somme
#pedro.fx[[3]] = vide
#pedro.fx[[4]] = nbVidesAvantPleine
#pedro.fx[[5]] = posMax


# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)


####################
# Première version #
####################

# On sélectionne toutes les observations correspondant aux coups joués par le gagnant de la partie
# On construit un modèle de Naive Bayes à partir des premières variables de ces observations
# On essaye de prédire la variable du coup joué

# Fonction de construction du modèle

pedro.create.model = function (dataset, listOfFx){
  # On complète les données
  dataset = addData.completeData(dataset, listOfFx)
  decal = addData.getDecalage(listOfFx)
  
  # On s?lectionne les instances qui correspondent aux coups joués par le vainqueur des affrontements  
  selection = dataset [dataset [, (14+decal)] == "G", ]
  
  # Et on construit un mod?le de classification avec l'algorithme Naive bayes
  model = naiveBayes (selection [, (1:12+decal)], selection [, (13+decal)])
  return (model)
}
# Construction du modèle
pedro.model = pedro.create.model (awele.data, listOfFx)


# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu et du modèle
pedro.exec = function (awele, model, listOfFx)
{
  # On récupère l'état du plateau de jeu (sous la forme d'une matrice plutÔt que d'un vecteur)
  g = graines.matrix (awele)
  
  # On ajoute les données
  g = addData.completeData(g, listOfFx)
  
  # On modifie les noms des colonnes pour correspondre aux noms dans l'ensemble d'apprentissage
  colnames (g)[1:12] = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  # On applique le mod?le et on retourne les pr?dictions (sous la forme de degr?s d'appartenance aux classes)
  return (predict (model, g, type = "raw"))
}
# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu (en utilisant la variable globale nb.model)
pedro = function (awele) return (pedro.exec (awele, pedro.model, listOfFx))
