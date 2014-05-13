# Chargement de la biblioth?que
library (e1071)
library("fdm2id") 
source("addData.r")

# Chargement des donn?es
awele.data = read.table ("awele.data", sep = ",", header = T)

######### Preparation des objets d'enrichissement de données
### Initialisation de la liste contenant les objets d'enrichisement
#######
## avec vide, sum1ou2, somme, posMax : bat SVM1 ( "svm1 (25 - 15)"  "svm2 (26 - 19)" )
## avec       sum1ou2, somme, posMax : egalite SVM1
## avec vide, sum1ou2,      , posMax : SVM1 gagne
## avec vide,        , somme, posMax : SVM1 gagne
## avec vide, sum1ou2, somme,        : SVM1 gagne
## avec vide, sum1ou2, somme, posMax, bidoua, nbVidesAvantPleine : bat SVM1 ( "svm1 (25 - 17)"  "svm2 (29 - 16)" )
## NE PAS rajouter unTour

listSVM = list()
i = 1;
listSVM[[i]] = vide

i = i + 1
listSVM[[i]] = somme

i = i + 1
listSVM[[i]] = sum1ou2

i = i + 1
listSVM[[i]] = posMax

i = i + 1
listSVM[[i]] = bidoua

i = i + 1
listSVM[[i]] = nbVidesAvantPleine


############################################
#   Botsvm (svm)  avec l'ajout des données #
############################################

###VERSION 1
# Fonction de construction du modèle
svm2.create.model = function (dataset)
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
svm2.model = svm2.create.model (awele.data)


svm2.exec = function (awele, model)
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
svm2 = function (awele) return (svm2.exec (awele, svm2.model))
