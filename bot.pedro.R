# Chargement de la bibliothèque
library (e1071)

# Chargement des fonction de addData
source("addData.r")

## Fonction qui rajoute les données au tableau existant
fx.completeData = function(data){
  
  # création d'une nouvelle frame avec 6 colonnes en plus
  decal = 6
  newData = data.frame(matrix(data=0, nr = nrow(data), nc=ncol(data)+ decal ))
  
  # ajout des colonnes de base
  for(i in 1:12){
    newData[,i] = data[,i];
    colnames(newData)[i] = colnames(data)[i]
  }
  
  # ajout des colonnes de somme, vides, et sum1ou2
  newData = addData(newData, 13, somme )
  newData = addData(newData, 15, vide)
  newData = addData(newData, 17, sum1ou2)  
  
  #ajout des colonnes finales
  for(i in 19:20){
    newData[,i] = data[,i-decal];
    colnames(newData)[i] = colnames(data)[i-decal]
  }
  
  return (newData)
}


# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)

# Ajout des données supplémentaires
awele.data = fx.completeData(awele.data)


####################
# Premi?re version #
####################

# On s?lectionne toutes les observations correspondant aux coups jou?s par le gagnant de la partie
# On construit un mod?le de Naive Bayes ? partir des 12 premi?res variables de ces observations
# On essaye de pr?dire la 13e variable (coup jou?)

# Fonction de construction du mod?les
pedro.create.model = function (dataset)
{
  # On s?lectionne les instances qui correspondent aux coups jou?s par le vainqueur des affrontements
  selection = awele.data [dataset [, 20] == "G", ]
  # Et on construit un mod?le de classification avec l'algorithme Naive bayes
  model = naiveBayes (selection [, 1:18], selection [, 19])
  return (model)
}
# Construction du mod?le
pedro.model = pedro.create.model (awele.data)
# Fonction d'?valuation de la meilleure solution selon l'?tat du plateau de jeu et du mod?le
pedro.exec = function (awele, model)
{
  # On récupère l'ztat du plateau de jeu (sous la forme d'une matrice plutÔt que d'un vecteur)
  g = graines.matrix (awele)
  
  ####
  ## on ajoute les colonnes sum, vides, 1ou2
  #### 
  # recopie de g
  g2 = data.frame(matrix(data=0, nr = nrow(g), nc=ncol(g)+6))
  
  # on ajoute les colonnes de base
  for(i in 1:12){
    g2[,i] = g[,i]
  }
  
  # sum
  g2 = addData(g2, 13, somme)
  g2 = addData(g2, 15, vide)
  g2 = addData(g2, 15, sum1ou2)
    
  g = g2;
  
  # On modifie les noms des colonnes pour correspondre aux noms dans l'ensemble d'apprentissage
  colnames (g)[1:12] = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  # On applique le mod?le et on retourne les pr?dictions (sous la forme de degr?s d'appartenance aux classes)
  return (predict (model, g, type = "raw"))
}
# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu (en utilisant la variable globale nb.model)
pedro = function (awele) return (pedro.exec (awele, pedro.model))


###################
# Seconde version #
###################

# On utilise toutes les observation
# On construit un modéle de Naive Bayes à partir des 13 premières variables de ces observations
# On essaye de pr?dire la 14e variable (coup gagnant ou perdant)
# Pour une nouvelle observation, on ne dispose que des 12 premi?re variables
# On fait la pr?diction pour chaque valeur possible ? la 13e variable

# Fonction de construction du mod?les
pedro2.create.model = function (dataset)
{
  # on construit un mod?le de classification avec l'algorithme Naive bayes
  model = naiveBayes (dataset [, 1:19], dataset [, 20])
  return (model)
}
# Construction du mod?le
pedro2.model = pedro2.create.model (awele.data)
# Fonction d'?valuation de la meilleure solution selon l'?tat du plateau de jeu et du mod?le
pedro2.exec = function (awele, model)
{
  # On r?cup?re l'?tat du plateau de jeu (sous la forme d'une matrice plut?t que d'un vecteur)
  g = graines.matrix (awele)
  # On r?p?te six fois l'?tat du plateau de jeu (et on transforme en data.frame)
  g = as.data.frame (g [rep (1, 6), ])
  # On ajoute une 13e colonne qui contient les six valeurs possibles
  g = cbind (g, factor (1:6, labels = levels (awele.data [, 19])))
  # On modifie les noms des colonnes pour correspondre aux noms dans l'ensemble d'apprentissage  
  colnames (g) = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""), "sumJ", "sumA", "videsJ", "videsA", "1ou2J","1ou2A","C")
  # On applique le mod?le
  #et on retourne le degr? d'appartenance ? la classe "G" (probabilit? d'apr?s NB que le coup soit gagnant)
  return (predict (model, g, type = "raw") [, "G"])
}
# Fonction d'?valuation de la meilleure solution selon l'?tat du plateau de jeu (en utilisant la variable globale nb.model)
pedro2 = function (awele) return (pedro2.exec (awele, pedro2.model))