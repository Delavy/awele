####################################
#   dpln.nbsupp (NB  + dataSupp)
###################################

####################
### Initialisation
####################

# Chargement des bibliothèques
library (e1071)
library (fdm2id)
source("dpln.addData.R")


######### Preparation des objets d'enrichissement de données
### Initialisation de la liste contenant les objets d'enrichisement
nbsupp.fx = list(sum1ou2)

# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)


#####################
### Création du modèle
#####################
dpln.nbsupp.create.model = function (dataset, listOfFx){
  # On complète les données
  dataset = addData.completeData(dataset, listOfFx)
  decal = addData.getDecalage(listOfFx)
  
  # On sélectionne les instances qui correspondent aux coups joués par le vainqueur des affrontements  
  selection = dataset [dataset [, (14+decal)] == "G", ]
  
  # Et on construit un mod?le de classification avec l'algorithme Naive bayes
  model = naiveBayes (selection [, (1:12+decal)], selection [, (13+decal)])
  return (model)
}
# Construction du modèle
dpln.nbsupp.model = dpln.nbsupp.create.model (awele.data, nbsupp.fx)


#####################
### Execution
#####################
dpln.nbsupp.exec = function (awele, model, listOfFx)
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
dpln.nbsupp = function (awele) return (dpln.nbsupp.exec (awele, dpln.nbsupp.model, nbsupp.fx))
