# Chargement de la biblioth?que
library (e1071)
library("fdm2id") 
# Chargement des donn?es
awele.data = read.table ("awele.data", sep = ",", header = T)


####################
#   mlp1 (MLP)     #
####################
# utilisation de nnet
###VERSION 1
#on prend que les gagnants
mlp1.create.model = function(dataset)
{
  tunecontrol2=tune.control(sampling="bootstrap", nboot=20, boot.size=1)    
  selection = dataset [dataset [, 14] == "G", ]  
  result = splitdata(dataset = selection,target = (13):(14), seed=0)  
  model = tune.nnet(result$train.x,result$train.y[,1], gamma=2^(-3:3), cost=2^(-3:3), tunecontrol=tunecontrol2, size=12)$best.model  
  tunecontrole=tune.control(sampling="bootstrap", nboot=20, boot.size=1)
  
  return(model)
}
mlp1.model =mlp1.create.model (awele.data)
