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
  selection = awele.data [dataset [, 14] == "G", ]
  print(selection)
  #c~. CORRESPOND A LA COLONNE
  model = nnet(selection[,1:12],selection[,13],size = 1)
  
  return(model)
}
mlp1.model =mlp1.create.model (awele.data)
