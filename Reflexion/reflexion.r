#--------------------------------------------------------
# Initialisation
#--------------------------------------------------------

# Chargement de la bibliothèque
library (e1071)
library (fdm2id)

# Fonction cout
cout = function(arg1, arg2='', arg3='',arg4=''){
  cat(arg1,arg2,arg3,arg4,'\n')
}

# Chargement des données
awele.data = read.table ("awele.data", sep = ",", header = T)
awele.val = awele.data[,1:12]
awele.gagne = awele.data[,14]
awele.coup = awele.data[,13]


#--------------------------------------------------------
#--  Ajout du nombre total de billes par joueur
#--------------------------------------------------------

# Création d'une nouvelle matrice qui contient les sommes
awele.sum = data.frame(matrix(data=0, nr = nrow(awele.data), nc=4 ))

# ajout des sommes
for(i in 1:nrow(awele.data)){
  awele.sum[i,1] = sum(awele.data[i,1:6])  
  awele.sum[i,2] = sum(awele.data[i,7:12])
}

awele.sum[,3] = awele.data[,13]
awele.sum[,4] = awele.data[,14]

#nom des colonnes
colnames(awele.sum)[1] = "sumJ"
colnames(awele.sum)[2] = "sumA"
colnames(awele.sum)[3] = "coup"
colnames(awele.sum)[4] = "gagne"

#Pearson : entre le gagne / perdu des vides
chisq.test(awele.sum[,1] , awele.coup)$p.value #0,78
chisq.test(awele.sum[,2] , awele.coup)$p.value #0,33

# recuperation de tous les coups gagnant 
selection = awele.sum [awele.gagne== "G",]
plot(selection[,1:2], col=selection[,3])

# On compare la relation entre le nombre de cases vides avec le coup joué pour les 2 joueurs
chisq.test(selection[,1] , selection[,3])$p.value #0,08
chisq.test(selection[,2] , selection[,3])$p.value #0,07
chisq.test((awele.sum[,1]-awele.sum[,2]) , awele.gagne)$p.value #0,001

#--------------------------------------------------------
#-- Ajout du nombre de cases vides par joueur
#--------------------------------------------------------

# Création d'une nouvelle matrice 
awele.vides = data.frame(matrix(data=0, nr = nrow(awele.data), nc=4))

# ajout du nombre de case à 0
for(i in 1:nrow(awele.data)){
  awele.vides[i,1] = sum(awele.data[i,1:6]==0)
  awele.vides[i,2] = sum(awele.data[i,7:12]==0)
}
# ajout des coups et du score
awele.vides[,3] = awele.data[,13]
awele.vides[,4] = awele.data[,14]

#nom des colonnes
colnames(awele.vides)[1] = "videsJ"
colnames(awele.vides)[2] = "videsA"
colnames(awele.vides)[3] = "coup"
colnames(awele.vides)[4] = "gagne"
plot(awele.vides, col=awele.gagne)

#Pearson : entre le gagne / perdu des vides
chisq.test(awele.vides[,1] , awele.coup)$p.value #0,0001
chisq.test(awele.vides[,2] , awele.coup)$p.value #0,29

# recuperation de tous les coups gagnant 
selection = awele.vides [awele.gagne== "G", ]
plot(selection[,1:2], col=selection[,3])

# On compare la relation entre le nombre de cases vides avec le coup joué pour les 2 joueurs
chisq.test(selection[,1] , selection[,3])$p.value #0,0022
chisq.test(selection[,2] , selection[,3])$p.value #0,0010

#--------------------------------------------------------
#-- Ajout du nombre de cases à 1 ou 2 par joueur
#--------------------------------------------------------

# Création d'une nouvelle matrice 
awele.1ou2 = data.frame(matrix(data=0, nr = nrow(awele.data), nc=4 ))

# ajout du nombre de case à 0
for(i in 1:nrow(awele.data)){
  awele.1ou2[i,1] = sum(awele.data[i,1:6]==2) + sum(awele.data[i,1:6]==1)
  awele.1ou2[i,2] = sum(awele.data[i,7:12]==2) + sum(awele.data[i,7:12]==1)
}
# ajout des colonnes de coup et de gagne
awele.1ou2[,3] = awele.data[,13]
awele.1ou2[,4] = awele.data[,14]
#nom des colonnes
colnames(awele.1ou2)[1] = "1ou2J"
colnames(awele.1ou2)[2] = "1ou2A"
colnames(awele.1ou2)[3] = "coup"
colnames(awele.1ou2)[4] = "gagne"

plot(awele.1ou2[,1:2], col=awele.gagne)

# Y a t'il un rapport entre le nombres de cases à 1 et à 2 et le coup joué ?
chisq.test(awele.1ou2[,1] , awele.coup)$p.value #0,66
chisq.test(awele.1ou2[,2] , awele.coup)$p.value #0,25

# recuperation de tous les coups gagnant 
selection = awele.1ou2 [awele.gagne== "G", ]
plot(selection[,1:2], col=selection[,3])

# On compare la relation entre le nombre de cases à 1ou2 avec le coup joué pour les 2 joueurs
chisq.test(selection[,1] , selection[,3])$p.value #0,092
chisq.test(selection[,2] , selection[,3])$p.value #0,24


#--------------------------------------------------------
# Résultat
#--------------------------------------------------------

### TEST la somme des 3 premières cases ?
### --> testé : non
### TEST la somme des 3 dernières cases ?
### --> testé : non

# en fonction de la somme ?
# en fonction du nombre de cases vides ?
# en foncion du nombre des case sà 1 ou à 2 chez ton adversaire ?
           
           

#---------------------------------------------------------------
#-- Parties pas sur
#---------------------------------------------------------------


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


