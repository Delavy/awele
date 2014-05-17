####################################
#   adlkiller (LDA)
###################################

# Chargement des bibliothèques
library (e1071)
library (fdm2id)
source("addData.r")

# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)

# Création de la liste des objets dataSupp
listOfKiller = list(vide, somme, posMax, nbGagne)


######################
### Création du modèle
######################
adlkiller.create.model = function (dataset)
{
  # Ajout des données
  dataset = addData.completeData(dataset, listOfKiller)
  # Calcul du décalage
  decal = addData.getDecalage(listOfKiller)  
  
  # On sélectionne les instances qui correspondent aux coups joués par le vainqueur des affrontements
  selection = dataset [dataset [, 14+decal] == "G", ]
    
  # Et on construit un modèle de classification avec l'algorithme ADL
  model = LDA(selection [, 1:(12+decal)], selection [, 13+decal])
  return (model)
}
# Construction du modèle
adlkiller.model = adlkiller.create.model (awele.data)


#####################
### Execution
#####################
adlkiller.exec = function (awele, model)
{
  # On récupère l'état du plateau de jeu (sous la forme d'une matrice plutôt que d'un vecteur)
  g = graines.matrix (awele)
  
  # ajout des données
  g = addData.completeData(g, listOfKiller)
  
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
adlkiller = function (awele) return (adlkiller.exec (awele, adlkiller.model))
