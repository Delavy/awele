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
  
  nb2 = naiveBayes (sp$train.x, sp$train.y [, 1])
  nb3 = naiveBayes (sp$train.x, sp$train.y [, 2])
  # Pour k-NN et SVM il faut utiliser la validation croisée (ici le bootstrap) et la configurer
  tunecontrol = tune.control(sampling = "bootstrap", nboot = 20, boot.size = 1)
  # On peut chercher le k de k-NN...
  k2 = tune.knn (sp$train.x, sp$train.y [, 1], k = 1:10, tunecontrol = tunecontrol)$best.model$k
  k3 = tune.knn (sp$train.x, sp$train.y [, 2], k = 1:10, tunecontrol = tunecontrol)$best.model$k
  # ... et le modèle des SVM
  svm2 = tune.svm (Class~., data = cbind (sp$train.x, Class = sp$train.y [, 1]), gamma = 2^(-3:3), cost = 2^(-3:3), tunecontrol = tunecontrol)$best.model
  svm3 = tune.svm (Class~., data = cbind (sp$train.x, Class = sp$train.y [, 2]), gamma = 2^(-3:3), cost = 2^(-3:3), tunecontrol = tunecontrol)$best.model
  # On fait les prédictions sur l'ensemble de test
  classif.nb2 = predict (nb2, sp$test.x)
  classif.nb3 = predict (nb3, sp$test.x)
  classif.knn2 = knn (sp$train.x, sp$test.x, sp$train.y [, 1], k = k2)
  classif.knn3 = knn (sp$train.x, sp$test.x, sp$train.y [, 2], k = k3)
  classif.svm2 = predict (svm2, sp$test.x)
  classif.svm3 = predict (svm3, sp$test.x)
  
  # Préparatoin de la matrice des résultats
  accuracy = as.data.frame(matrix(ncol=3, nrow=2))
  kappa = as.data.frame(matrix(ncol=3, nrow=2))
  
  colnames(accuracy) = c("acc-nb", "acc-knn", "acc-svm")
  rownames(accuracy) = c("2c","3c")
  colnames(kappa) = c("kappa-nb", "kappa-knn", "kappa-svm")
  rownames(kappa) = c("2c","3c")
  
  # Et on compare avec les classes réelles selon le taux de succès
  accuracy[1,1] = evaluation (classif.nb2, sp$test.y [, 1], "accuracy")
  accuracy[2,1] = evaluation (classif.nb3, sp$test.y [, 2], "accuracy")
  accuracy[1,2] = evaluation (classif.knn2, sp$test.y [, 1], "accuracy")
  accuracy[2,2] = evaluation (classif.knn3, sp$test.y [, 2], "accuracy")
  accuracy[1,3] = evaluation (classif.svm2, sp$test.y [, 1], "accuracy")
  accuracy[2,3] = evaluation (classif.svm3, sp$test.y [, 2], "accuracy")
  
  # Et on compare avec les classes réelles selon le coefficient de Cohen
  kappa[1,1] = evaluation (classif.nb2, sp$test.y [, 1], "kappa")
  kappa[2,1] = evaluation (classif.nb3, sp$test.y [, 2], "kappa")
  kappa[1,2] = evaluation (classif.knn2, sp$test.y [, 1], "kappa")
  kappa[2,2] = evaluation (classif.knn3, sp$test.y [, 2], "kappa")
  kappa[1,3] = evaluation (classif.svm2, sp$test.y [, 1], "kappa")
  kappa[2,3] = evaluation (classif.svm3, sp$test.y [, 2], "kappa")
  
  
  resultat = cbind(accuracy, kappa)
  return (resultat)
}


# On lance l'évaluation sur les données classiques
score1 = evalclassif(data, 13:14)

# On ajoute les données et on test
listOfFx = list(sum1ou2)
dataB = addData.completeData(data, listOfFx)
decal = addData.getDecalage(listOfFx)
score2 = evalclassif(dataB, (13+decal):(14+decal))

# On ajoute les données et on test
listOfFx = list(vide)
dataB = addData.completeData(data, listOfFx)
decal = addData.getDecalage(listOfFx)
score3 = evalclassif(dataB, (13+decal):(14+decal))
