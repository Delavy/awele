# Chargement de la bibliothèque
library (e1071)

# Chargement des fonction de addData
source("addData.r")


######### Preparation des objets d'enrichissement de données
### Initialisation de la liste contenant les objets d'enrichisement
pedro.fx = list()
pedro.fx[[1]] = sum1ou2
#pedro.fx[[2]] = somme
#pedro.fx[[3]] = vide

### Calcul du décalage apporté par la liste
pedro.decalage = 0
for(i in 1:length(pedro.fx)){
  pedro.decalage = pedro.decalage + pedro.fx[[i]]@decal
}



########
### Function qui rajoute les données au tableau existant
### @param data : les données. 
### @return : les données avec les colonnes rajoutées
fx.completeData = function(data){
  
  # récupération du décalage
  decal = pedro.decalage
  
  # création d'une nouvelle frame avec les colonnes supplémentaires
  newData = data.frame(matrix(data=0, nr = nrow(data), nc=ncol(data)+ decal ))
  
  # Recopie des colonnes de base, ainsi que des nom si il y en as
  asColNames = length(colnames(data))>0;  
  for(i in 1:12){
    newData[,i] = data[,i];
    if(asColNames){
      colnames(newData)[i] = colnames(data)[i]
    }
  }
  
  # Ajout des données par execution des fonctions
  pos = 13
  # execution des fonctions 1 par 1
  for(i in 1:length(pedro.fx)){
    # on execute la fonction
    newData = pedro.fx[[i]]@fonction(newData, pos)
    #on ajoute le décalage apporté par la fonction
    pos = pos + pedro.fx[[i]]@decal
  }
      
    
  # si il reste des colonnes
  if(ncol(newData)>(12+decal)){    
    #ajout des colonnes finales
    for(i in (13+decal):ncol(newData)){
      newData[,i] = data[,i-decal];
      colnames(newData)[i] = colnames(data)[i-decal]
    }
  }
  
  return (newData)
}
########


# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)

# Ajout des données supplémentaires  
#awele.data = fx.completeData(awele.data)


####################
# Première version #
####################

# On sélectionne toutes les observations correspondant aux coups joués par le gagnant de la partie
# On construit un modèle de Naive Bayes à partir des premières variables de ces observations
# On essaye de prédire la variable du coup joué

# Fonction de construction du modèle

pedro.create.model = function (dataset){
  dataset = fx.completeData(dataset)
  decal = pedro.decalage
  # On s?lectionne les instances qui correspondent aux coups joués par le vainqueur des affrontements
  
  selection = dataset [dataset [, (14+decal)] == "G", ]
  # Et on construit un mod?le de classification avec l'algorithme Naive bayes
  model = naiveBayes (selection [, (1:12+decal)], selection [, (13+decal)])
  return (model)
}
# Construction du modèle
pedro.model = pedro.create.model (awele.data)


# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu et du modèle
pedro.exec = function (awele, model)
{
  decal = pedro.decalage
  # On récupère l'état du plateau de jeu (sous la forme d'une matrice plutÔt que d'un vecteur)
  g = graines.matrix (awele)
  
  # On ajoute les données
  g = fx.completeData(g)
  
  # On modifie les noms des colonnes pour correspondre aux noms dans l'ensemble d'apprentissage
  colnames (g)[1:12] = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  # On applique le mod?le et on retourne les pr?dictions (sous la forme de degr?s d'appartenance aux classes)
  return (predict (model, g, type = "raw"))
}
# Fonction d'évaluation de la meilleure solution selon l'état du plateau de jeu (en utilisant la variable globale nb.model)
pedro = function (awele) return (pedro.exec (awele, pedro.model))
