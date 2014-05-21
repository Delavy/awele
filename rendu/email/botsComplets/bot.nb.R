# Chargement de la bibliothèque
library (e1071)
# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)

####################
# Première version #
####################

# On sélectionne toutes les observations correspondant aux coups joués par le gagnant de la partie
# On construit un modèle de Naive Bayes à partir des 12 premières variables de ces observations
# On essaye de prédire la 13e variable (coup joué)

# Fonction de construction du modèle
nb.create.model = function (dataset)
{
  # On sélectionne les instances qui correspondent aux coups joués par le vainqueur des affrontements
  selection = awele.data [dataset [, 14] == "G", ]
  # Et on construit un modèle de classification avec l'algorithme Naive bayes
  model = naiveBayes (selection [, 1:12], selection [, 13])
  return (model)
}
# Construction du modèle
nb.model = nb.create.model (awele.data)
# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu et du modèle
nb.exec = function (awele, model)
{
  # On récupère l'état du plateau de jeu (sous la forme d'une matrice plutôt que d'un vecteur)
  g = graines.matrix (awele)
  # On modifie les noms des colonnes pour correspondre aux noms dans l'ensemble d'apprentissage
  colnames (g) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  # On applique le modèle et on retourne les prédictions (sous la forme de degrés d'appartenance aux classes)
  return (predict (model, g, type = "raw"))
}
# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu (en utilisant la variable globale nb.model)
nb = function (awele) return (nb.exec (awele, nb.model))

###################
# Seconde version #
###################

# On utilise toutes les observations
# On construit un modèle de Naive Bayes à partir des 13 premières variables de ces observations
# On essaye de prédire la 14e variable (coup gagnant ou perdant)
# Pour une nouvelle observation, on ne dispose que des 12 première variables
# On fait la pr?diction pour chaque valeur possible à la 13e variable

# Fonction de construction du modèle
nb2.create.model = function (dataset)
{
  # on construit un modèle de classification avec l'algorithme Naive bayes
  model = naiveBayes (dataset [, 1:13], dataset [, 14])
  return (model)
}
# Construction du modèle
nb2.model = nb2.create.model (awele.data)
# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu et du modèle
nb2.exec = function (awele, model)
{
  # On récupère l'état du plateau de jeu (sous la forme d'une matrice plutôt que d'un vecteur)
  g = graines.matrix (awele)
  c(typeof(g))
  # On répète six fois l'état du plateau de jeu (et on transforme en data.frame)
  g = as.data.frame (g [rep (1, 6), ])
  # On ajoute une 13e colonne qui contient les six valeurs possibles
  g = cbind (g, factor (1:6, labels = levels (awele.data [, 13])))
  # On modifie les noms des colonnes pour correspondre aux noms dans l'ensemble d'apprentissage  
  colnames (g) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""), "C")
  # On applique le modèle
  #et on retourne le degré d'appartenance à la classe "G" (probabilité d'après NB que le coup soit gagnant)
  return (predict (model, g, type = "raw") [, "G"])
}
# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu (en utilisant la variable globale nb.model)
nb2 = function (awele) return (nb2.exec (awele, nb2.model))