####################################
#   dpln.svmsupp ( ADL + données )
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

######### Preparation des objets d'enrichissement de données
### Initialisation de la liste contenant les objets d'enrichisement
#######
## avec vide, sum1ou2, somme, posMax : bat SVM1 ( "svm1 (25 - 15)"  "dpln.svmsupp (26 - 19)" )
## avec       sum1ou2, somme, posMax : egalite SVM1
## avec vide, sum1ou2,      , posMax : SVM1 gagne
## avec vide,        , somme, posMax : SVM1 gagne
## avec vide, sum1ou2, somme,        : SVM1 gagne
## avec vide, sum1ou2, somme, posMax, bidoua, nbVidesAvantPleine : bat SVM1 ( "svm1 (25 - 17)"  "dpln.svmsupp (29 - 16)" )
## NE PAS rajouter unTour

listSVM = list(vide, somme, sum1ou2, posMax, bidoua, nbVidesAvantPleine)

#####################
### Création du modèle
#####################
dpln.svmsupp.create.model = function (dataset)
{
  dataset = addData.completeData(dataset, listSVM)
  decal = addData.getDecalage(listSVM)
  
  tunecontrol2=tune.control(sampling="bootstrap", nboot=20, boot.size=1)    
  selection = dataset [dataset [, 14+decal] == "G", ]  
  result = splitdata(dataset = selection,target = (13+decal):(14+decal), seed=0)  
  model = tune.svm(result$train.x,result$train.y[,1], gamma=2^(-3:3), cost=2^(-3:3), tunecontrol=tunecontrol2)$best.model  
  return (model)
}
# Construction du modèle
dpln.svmsupp.model = dpln.svmsupp.create.model (awele.data)

#####################
### Execution
#####################
dpln.svmsupp.exec = function (awele, model)
{  
  g = graines.matrix (awele)
  g = addData.completeData(g, listSVM)
  colnames (g)[1:12] = c (paste ("J", 1:6, sep = ""), paste ("A", 1:6, sep = ""))
  prediction = predict (model, data.frame(g),type = "raw")
  
  # On crée une nouvelle dataframe pour le résultat, tout à 0, et on y colle les noms des colonnes
  ret = data.frame(matrix(data=0, ncol=6, nrow=1))
  colnames(ret) = levels(prediction)
  
  # on set la colonne retournée par prédict ) 1.
  ret[c(prediction[1])] = 1
  return (ret)
}
# Fonction d'?valuation de la meilleure solution selon l'?tat du plateau de jeu (en utilisant la variable globale nb.model)
dpln.svmsupp = function (awele) return (dpln.svmsupp.exec (awele, dpln.svmsupp.model))
