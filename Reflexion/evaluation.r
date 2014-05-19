########################################
#Evaluation selon la méthode du TD4
########################################

# Bibliothèques
library (e1071)
library("fdm2id") 
source("addData.r")

# Chargement des données
data = read.table ("awele.data", sep = ",", header = T)

#Fonction
evalclassif = function(data, libelles){

  
  # Découpage
  sp = splitdata (data, libelles, seed = 0)
  
  # Et on construit un modèle de classification avec l'algorithme ADL
  adl2 = LDA(sp$train.x, sp$train.y [, 1])
  adl3 = LDA(sp$train.x, sp$train.y [, 2])
  
  classif.adl2 = predict (adl2, sp$test.x)
  classif.adl3 = predict (adl3, sp$test.x)
  
  
  # Préparatoin de la matrice des résultats
  accuracy = as.data.frame(matrix(ncol=1, nrow=2))
  kappa = as.data.frame(matrix(ncol=1, nrow=2))
  
  colnames(accuracy) = c("accur-adl")
  rownames(accuracy) = c("2c","3c")
  colnames(kappa) = c("kappa-adl")
  rownames(kappa) = c("2c","3c")
  
  # Et on compare avec les classes réelles selon le taux de succès
  accuracy[1,1] = evaluation (classif.adl2, sp$test.y [, 1], "accuracy")
  accuracy[2,1] = evaluation (classif.adl3, sp$test.y [, 2], "accuracy")
  
  # Et on compare avec les classes réelles selon le coefficient de Cohen
  kappa[1,1] = evaluation (classif.adl2, sp$test.y [, 1], "kappa")
  kappa[2,1] = evaluation (classif.adl3, sp$test.y [, 2], "kappa")
  
  
  resultat = cbind(accuracy, kappa)
  return (resultat)
}

evalListInADL = function(data, listDeFx, nom, resultat){
  dataB = addData.completeData(data, listDeFx)
  decal = addData.getDecalage(listDeFx)
  res = evalclassif(dataB, (13+decal):(14+decal))
  colnames(res) = c(paste("accur-",nom),paste("kappa-",nom))
  resultat = cbind(resultat, res)  
  return(resultat)
}

resultat = evalclassif(data, 13:14)
resultat = evalListInADL(data, list(nulle), "nulle", resultat)
resultat = evalListInADL(data, list(bidoua), "bidoua", resultat)
resultat = evalListInADL(data, list(matrice1ou2), "matrice1ou2", resultat)
resultat = evalListInADL(data, list(nbGagne), "nbGagne", resultat)
resultat = evalListInADL(data, list(nbVidesAvantPleine), "nbVidesAvantPleine", resultat)
resultat = evalListInADL(data, list(posMax), "posMax", resultat)
resultat = evalListInADL(data, list(somme), "somme", resultat)
resultat = evalListInADL(data, list(sum1ou2), "sum1Ou2", resultat)
resultat = evalListInADL(data, list(unTour), "unTour", resultat)
resultat = evalListInADL(data, list(vide), "vide", resultat)
resultat = evalListInADL(data, list(vide,bidoua), "bidou-vide", resultat)
resultat = evalListInADL(data, list(vide,matrice1ou2), "matr12-vide", resultat)
resultat = evalListInADL(data, list(vide, somme, posMax, nbGagne), "killer", resultat)

##### Evaluation à l'aide de 
# cost.Curves


# Chargement du jeu d'Aw?l? en R
source ("awele.R")

source ("bot.nb.R")       #nb + nb2
source ("bot.random.R")   #random

source ("bot.pedro.R")    #pedro
source ("bot.pedrobis.R") #pedrobis

source ("bot.adl.R")      #botADL1
source ("bot.adl12.R")     #adl12
source ("bot.adlsum.R")     #adlsum
source ("bot.adlkiller.R")  #adlkiller

source ("bot.afd.R")      #afd1
source ("bot.rl.R")       #RL
source ("bot.knn.R")      # knn1

source ("bot.ludo.R")     #mlpludo
source ("bot.mlpbis.R")   #mlpbis

source ("bot.cart.R")     # cart1 
source ("bot.cartbis.R")  # cartBis


##### Attention, prennent du temps à charger
source ("bot.svm2.R")   # svm2
source ("bot.svm.R")    # svm

plot (data [, 1:12], col = data [, 13], asp = 1)
data [, 1:12] = PCA (data [, 1:12])$ind$coord

#debut des tests
bootstrap.curves(method=naiveBayes,x=data[,1:12],y=data[,13], curve="ROCR",new=T,seed=0,lty=1)



