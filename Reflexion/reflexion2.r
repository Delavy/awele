#--------------------------------------------------------
# Initialisation
#--------------------------------------------------------

# Chargement de la biblioth?que
library (e1071)
library (fdm2id)
# Chargement des fonction de addData
source("addData.r")

# Fonction cout
cout = function(arg1, arg2='', arg3='',arg4=''){
  cat(arg1,arg2,arg3,arg4,'\n')
}

# Chargement des donn?es
# !! Attention, on doit etre plac? dans le bon repertoire pour charger le fichier awele.data
# !! Pour changer : ctrl + maj + j
awele.data = read.table ("awele.data", sep = ",", header = T)
# !! 
awele.val = awele.data[,1:12]
awele.gagne = awele.data[,14]
awele.coup = awele.data[,13]

listOfFx = list()
listOfFx[[1]] = sum1ou2
listOfFx[[2]] = somme
listOfFx[[3]] = vide
listOfFx[[3]] = posMax
listOfFx[[5]] = bidoua
decal = addData.getDecalage(listOfFx)

awele.data = addData.completeData(awele.data, listOfFx)

awele.pca = PCA (awele.data, quali.sup = 15:16, ncp=10)
barplot (awele.pca$eig [, 3])
print (sum (awele.pca$eig [, 1] > 1)) # 4
