# Chargement de la biblioth�que
library (e1071)

cout = function(arg1, arg2='', arg3='',arg4=''){
  cat(arg1,arg2,arg3,arg4,'\n')
}


# Chargement des donn�es
awele.data = read.table ("awele.data", sep = ",", header = T)
awele.val = awele.data[,1:12]


########
#### Ajout du nombre total de billes par joueur
#######

# Cr�ation d'une nouvelle matrice
# * m�me nombre de lignes que awele
# * 2 colonnes de plus
newdata = matrix(data=0, nr = nrow(awele.data), nc=ncol(awele.data)+2 )
newdata = data.frame(newdata)
#affectation des donn�es : les 12 premi�res, 2 vide, les 2 derni�res
for (i in 1:12){
  newdata[,i] = awele.data[,i]
  colnames(newdata)[i] = colnames(awele.data)[i]
}

# d�calage de 2
for (i in 13:14){
  newdata[,i+2] = awele.data[,i]
  colnames(newdata)[i+2] = colnames(awele.data)[i]
}


# ajout des sommes
for(i in 1:nrow(newdata)){
  newdata[i,13] = sum(newdata[i,1:6])  
  newdata[i,14] = sum(newdata[i,7:12])
}
awele.data = newdata


########
#Voir les donn�es
########
plot(awele.data[,1:14], col=awele.data[,16])

########
#Les attributs sont ils ind�pendants ? Utiliser le Test de Pearson
########
chisq.test(awele.data[,1], awele.data[,12])$p.value
cout("ca sert � rien")

########
####Les attributs sont ils corr�l�s lin�airements ?
########
correlations = cor (awele.val)
#on el�ve la diagonale
diag (correlations) = 0
# Et on r�cup�re la valeur absolue
correlations = abs (correlations)
# On les affiches
print (correlations)


########
#### ACP ?
########
acp = PCA(awele.val, quanti.sup=awele.data[,16])
plot.PCA(acp)
cout("ca sert � rien !")


########
#### NB 
#######
# On s�lectionne les instances qui correspondent aux coups jou�s par le vainqueur des affrontements
selection = awele.data [awele.data [, 16] == "G", ]
# Et on construit un mod�le de classification avec l'algorithme Naive bayes
model = naiveBayes (selection [, 1:12], selection [, 15])


########
#### Bootstrap
#######
vseed=0;
vnruns=100;
data.val = selection[,1:12]
data.pca = PCA(data.val)
data.name = selection[,15]

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


########
#### Y a t'il un rapport entre la diff�rente de billes J1 et J2 et le coup gagn� ou perdu ?
#######
# affichage des valeurs et de la couleur selon gagn� et perdu
plot(awele.data[,13:14], col=awele.data[,16])
chisq.test((awele.data[,13]-awele.data[,14], awele.data[,16])$p.value
           
# la somme des 3 premi�res cases ?
for(i in 1:nrow(awele.data)){
  awele.data[i,13] = sum(awele.data[i,4:6])  
  awele.data[i,14] = sum(awele.data[i,10:12])
}
plot(awele.data[,13], col=awele.data[,16])
# en fonction du nombre de cases vides !!!!!!
# en foncion du nombre des case s� 1 ou � 2 chez ton adversaire

