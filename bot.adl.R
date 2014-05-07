# Chargement de la biblioth?que
library (e1071)
library (fdm2id)
# Chargement des donn?es
awele.data = read.table ("awele.data", sep = ",", header = T)

####################
# Première version #
####################

# On sélectionne toutes les observations correspondant aux coups joués par le gagnant de la partie
# On construit un modèle d'ADL à partir des 12 premières variables de ces observations
# On essaye de prédire la 13e variable (coup joué)

# Fonction de construction du modèles
botADL1.create.model = function (dataset)
{
  # On sélectionne les instances qui correspondent aux coups joués par le vainqueur des affrontements
  selection = awele.data [dataset [, 14] == "G", ]
  # Et on construit un modèle de classification avec l'algorithme ADL
  model = LDA(selection [, 1:12], selection [, 13])
  return (model)
}
# Construction du mod?le
botADL1.model = botADL1.create.model (awele.data)
# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu et du modèle
botADL1.exec = function (awele, model)
{
  # On récupère l'état du plateau de jeu (sous la forme d'une matrice plutôt que d'un vecteur)
  g = graines.matrix (awele)
  # On modifie les noms des colonnes pour correspondre aux noms dans l'ensemble d'apprentissage
  colnames (g) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  # On applique le modèle et on retourne les pr?dictions (sous la forme de degrés d'appartenance aux classes)
  prediction = predict (model, data.frame(g),type = "raw")
  # tableau à 0
  ret = data.frame(matrix(data=0, ncol=6, nrow=1))
  # on remplace les noms des colonnes par C1,... les colonnes du plateau
  colnames(ret) = levels(prediction)
  # on met la colonne retournée par predict
  ret[c(prediction[1])] = 1
  return (ret)
}
# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu (en utilisant la variable globale nb.model)
botADL1 = function (awele) return (botADL1.exec (awele, botADL1.model))

####################
# Deuxième version #
####################
# On utilise toutes les observation
# On construit un modèle d'ADL à partir des 13 premières variables de ces observations
# On essaye de prédire la 14e variable (coup gagnant ou perdant)
# Pour une nouvelle observation, on ne dispose que des 12 première variables
# On fait la prédiction pour chaque valeur possible à la 13e variable
fakegraines = function()  return (data.frame(matrix(data=4,ncol=12,nrow=1))) 
# Fonction de construction du modèle ADL
botADL2.create.model = function (dataset)
{
  model = LDA (dataset [, 1:13], dataset [, 14])
  return (model)
}
# Construction du modèle
botADL2.model = botADL2.create.model (awele.data)
# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu et du modèle
botADL2.exec = function (awele, model)
{
  # On récupère l'état du plateau de jeu (sous la forme d'une matrice plutôt que d'un vecteur)
  g2 = graines.matrix (awele)
  # On répète six fois l'état du plateau de jeu (et on transforme en data.frame)
  g2 = as.data.frame (g2 [rep (1, 6), ])
  
  # On ajoute une 13e colonne qui contient les six valeurs possibles
  g2 = cbind (g2, factor (1:6, labels = levels (awele.data [, 13])))
  # On modifie les noms des colonnes pour correspondre aux noms dans l'ensemble d'apprentissage  
  colnames (g2) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""), "C")
  # On applique le modèle
  #et on retourne le degré d'appartenance à la classe "G" (probabilité d'après ADL que le coup soit gagnant)
  tmp = (predict (model, data.frame(g2),type = "raw") [ ,"G"])
  # tableau à 0
  ret = data.frame(matrix(data=0, ncol=6, nrow=1))
  # on remplace les noms des colonnes par C1,... les colonnes du plateau
  colnames(ret) = levels(tmp)
  # on met la colonne retourné par predict
  ret[c(tmp[1])] = 1
  return (ret)
}
# Fonction d'évaluation de la meilleure solution selon l'?tat du plateau de jeu (en utilisant la variable globale botADL2.model)
botADL2 = function (awele) return (botADL2.exec (awele, botADL2.model))
