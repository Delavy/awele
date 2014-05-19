####################################
#   dpln.adlsupp ( ADL)
# 	bots :  
#     * dpln.adl12
#     * dpln.adlsum
#     * dpln.adlkiller
###################################

####################
### Initialisation
####################

# Chargement des bibliothèques
library (e1071)
library (fdm2id)
source("dpln.addData.R")

# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)


#####################
### Création du modèle
#####################
dpln.adlsupp.create.model = function (dataset, listOfFx)
{
  dataset = addData.completeData(dataset, listOfFx)
  decal = addData.getDecalage(listOfFx)  
  
  # On sélectionne les instances qui correspondent aux coups joués par le vainqueur des affrontements
  selection = dataset [dataset [, 14+decal] == "G", ]
  # Et on construit un modèle de classification avec l'algorithme ADL
  model = LDA(selection [, 1:(12+decal)], selection [, 13+decal])
  return (model)
}

#####################
### Execution
#####################

# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu et du modèle
dpln.adlsupp.exec = function (awele, model, listOfFx)
{
  # On récupère l'état du plateau de jeu (sous la forme d'une matrice plutôt que d'un vecteur)
  g = graines.matrix (awele)
  
  # on ajoute les données
  g = addData.completeData(g, listOfFx)
  
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

#####################
### Différents bots 
#####################


# Liste de fonctions
adl12.fx      = list(sum1ou2)
adlsum.fx     = list(somme)
adlkiller.fx  = list(vide, somme, posMax, nbGagne)

# Construction du modèle
dpln.adl12.model      = dpln.adlsupp.create.model (awele.data, adl12.fx)
dpln.adlsum.model     = dpln.adlsupp.create.model (awele.data, adlsum.fx)
dpln.adlkiller.model  = dpln.adlsupp.create.model (awele.data, adlkiller.fx)

# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu (en utilisant la variable globale nb.model)
dpln.adl12      = function (awele) return (dpln.adlsupp.exec (awele, dpln.adl12.model, adl12.fx))
dpln.adlsum     = function (awele) return (dpln.adlsupp.exec (awele, dpln.adlsum.model, adlsum.fx))
dpln.adlkiller  = function (awele) return (dpln.adlsupp.exec (awele, dpln.adlkiller.model, adlkiller.fx))
