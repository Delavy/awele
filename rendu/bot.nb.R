# Chargement de la biblioth?que
library (e1071)
# Chargement des donn?es
awele.data = read.table ("awele.data", sep = ",", header = T)

####################
# Premi?re version #
####################

# On s?lectionne toutes les observations correspondant aux coups jou?s par le gagnant de la partie
# On construit un mod?le de Naive Bayes ? partir des 12 premi?res variables de ces observations
# On essaye de pr?dire la 13e variable (coup jou?)

# Fonction de construction du mod?les
nb.create.model = function (dataset)
{
  # On s?lectionne les instances qui correspondent aux coups jou?s par le vainqueur des affrontements
  selection = awele.data [dataset [, 14] == "G", ]
  # Et on construit un mod?le de classification avec l'algorithme Naive bayes
  model = naiveBayes (selection [, 1:12], selection [, 13])
  return (model)
}
# Construction du mod?le
nb.model = nb.create.model (awele.data)
# Fonction d'?valuation de la meilleure solution selon l'?tat du plateau de jeu et du mod?le
nb.exec = function (awele, model)
{
  # On r?cup?re l'?tat du plateau de jeu (sous la forme d'une matrice plut?t que d'un vecteur)
  g = graines.matrix (awele)
  # On modifie les noms des colonnes pour correspondre aux noms dans l'ensemble d'apprentissage
  colnames (g) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  # On applique le mod?le et on retourne les pr?dictions (sous la forme de degr?s d'appartenance aux classes)
  return (predict (model, g, type = "raw"))
}
# Fonction d'?valuation de la meilleure solution selon l'?tat du plateau de jeu (en utilisant la variable globale nb.model)
nb = function (awele) return (nb.exec (awele, nb.model))

###################
# Seconde version #
###################

# On utilise toutes les observation
# On construit un mod?le de Naive Bayes ? partir des 13 premi?res variables de ces observations
# On essaye de pr?dire la 14e variable (coup gagnant ou perdant)
# Pour une nouvelle observation, on ne dispose que des 12 premi?re variables
# On fait la pr?diction pour chaque valeur possible ? la 13e variable

# Fonction de construction du mod?les
nb2.create.model = function (dataset)
{
  # on construit un mod?le de classification avec l'algorithme Naive bayes
  model = naiveBayes (dataset [, 1:13], dataset [, 14])
  return (model)
}
# Construction du mod?le
nb2.model = nb2.create.model (awele.data)
# Fonction d'?valuation de la meilleure solution selon l'?tat du plateau de jeu et du mod?le
nb2.exec = function (awele, model)
{
  # On r?cup?re l'?tat du plateau de jeu (sous la forme d'une matrice plut?t que d'un vecteur)
  g = graines.matrix (awele)
  c(typeof(g))
  # On r?p?te six fois l'?tat du plateau de jeu (et on transforme en data.frame)
  g = as.data.frame (g [rep (1, 6), ])
  # On ajoute une 13e colonne qui contient les six valeurs possibles
  g = cbind (g, factor (1:6, labels = levels (awele.data [, 13])))
  # On modifie les noms des colonnes pour correspondre aux noms dans l'ensemble d'apprentissage  
  colnames (g) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""), "C")
  # On applique le mod?le
  #et on retourne le degr? d'appartenance ? la classe "G" (probabilit? d'apr?s NB que le coup soit gagnant)
  return (predict (model, g, type = "raw") [, "G"])
}
# Fonction d'?valuation de la meilleure solution selon l'?tat du plateau de jeu (en utilisant la variable globale nb.model)
nb2 = function (awele) return (nb2.exec (awele, nb2.model))