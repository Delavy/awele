# Chargement de la bibliothèque
library (e1071)
# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)
awele.val = awele.data[,1:12]

cout = function(arg1, arg2='', arg3='',arg4=''){
  cat(arg1,arg2,arg3,arg4,'\n')
}

########
#Voir les données
########
plot(awele.data[,1:12], col=awele.data[,14])

########
#Les attributs sont ils indépendants ? Utiliser le Test de Pearson
########
chisq.test(awele.data[,1], awele.data[,12])$p.value
cout("ca sert à rien")

########
####Les attributs sont ils corrélés linéairements ?
########
correlations = cor (awele.val)
#on elève la diagonale
diag (correlations) = 0
# Et on récupère la valeur absolue
correlations = abs (correlations)
# On les affiches
print (correlations)


########
#### ACP ?
########
acp = PCA(awele.val, quanti.sup=awele.data[,14])
plot.PCA(acp)
cout("ca sert à rien !")

pl