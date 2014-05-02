# Chargement de la biblioth?que
library (e1071)
library (fdm2id)
# Chargement des donn?es
awele.data = read.table ("awele.data", sep = ",", header = T)

####################
# Premi?re version #
####################

# On s?lectionne toutes les observations correspondant aux coups jou?s par le gagnant de la partie
# On construit un mod?le de Naive Bayes ? partir des 12 premi?res variables de ces observations
# On essaye de pr?dire la 13e variable (coup jou?)

# Fonction de construction du mod?les
ludo.create.model = function (dataset)
{
  # On s?lectionne les instances qui correspondent aux coups jou?s par le vainqueur des affrontements
  selection = awele.data [dataset [, 14] == "G", ]
  # Et on construit un mod?le de classification avec l'algorithme Naive bayes
  #model = naiveBayes (selection [, 1:12], selection [, 13])
  model = LDA(selection [, 1:12], selection [, 13])
  return (model)
}
# Construction du mod?le
ludo.model = ludo.create.model (awele.data)
# Fonction d'?valuation de la meilleure solution selon l'?tat du plateau de jeu et du mod?le
ludo.exec = function (awele, model)
{
  # On r?cup?re l'?tat du plateau de jeu (sous la forme d'une matrice plut?t que d'un vecteur)
  g = graines.matrix (awele)
  # On modifie les noms des colonnes pour correspondre aux noms dans l'ensemble d'apprentissage
  colnames (g) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  # On applique le mod?le et on retourne les pr?dictions (sous la forme de degr?s d'appartenance aux classes)
  prediction = predict (model, data.frame(g),type = "raw")
  # tableau à 0
  ret = data.frame(matrix(data=0, ncol=6, nrow=1))
  # on remplace les noms des colonnes par C1,... les colonnes du plateau
  colnames(ret) = levels(prediction)
  # on met la colonne re
  ret[c(prediction[1])] = 1
  return (ret)
}
# Fonction d'?valuation de la meilleure solution selon l'?tat du plateau de jeu (en utilisant la variable globale nb.model)
ludo = function (awele) return (ludo.exec (awele, ludo.model))

