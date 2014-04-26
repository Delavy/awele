# Chargement de la bibliothèque
library (e1071)

cout = function(arg1, arg2='', arg3='',arg4=''){
  cat(arg1,arg2,arg3,arg4,'\n')
}


# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)
awele.val = awele.data[,1:12]


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


########
#### NB 
#######
# On sélectionne les instances qui correspondent aux coups joués par le vainqueur des affrontements
selection = awele.data [awele.data [, 14] == "G", ]
# Et on construit un modèle de classification avec l'algorithme Naive bayes
model = naiveBayes (selection [, 1:12], selection [, 13])


########
#### Bootstrap
#######
vseed=0;
vnruns=100;
data.val = selection[,1:12]
data.pca = PCA(data.val)
data.name = selection[,13]

resultat = c (0, 0, 0, 0)
#bootstrap : 026 : ridicule 
resultat[1] = bootstrap(method=naiveBayes,x=data.val,y=data.name, seed=vseed, nruns=vnruns)
names(resultat)[1] = "NB"

#ALD 5
resultat[2] = bootstrap(method=LDA,x=data.val, y=data.name, seed=vseed, nruns=vnruns)
names(resultat)[2] = "ALD"

#AFD 
resultat[3] =  bootstrap(method=CDA,x=data.val, y=data.name, seed=vseed, nruns=vnruns)
names(resultat)[3] = "AFD"

#RL : 
resultat[4] = bootstrap(method=LR,x=data.val, y=data.name, seed=vseed, nruns=vnruns)
names(resultat)[4] = "RL"

#tri des resultats
resultat = sort(resultat);
barplot(resultat)
