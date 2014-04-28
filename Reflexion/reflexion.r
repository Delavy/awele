#--------------------------------------------------------
# Initialisation
#--------------------------------------------------------

# Chargement de la biblioth?que
library (e1071)
library (fdm2id)

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


#--------------------------------------------------------
#--  Ajout du nombre total de billes par joueur
#--------------------------------------------------------

# Cr?ation d'une nouvelle matrice qui contient les sommes
awele.sum = data.frame(matrix(data=0, nr = nrow(awele.data), nc=4 ))

# ajout des sommes
for(i in 1:nrow(awele.data)){
  awele.sum[i,1] = sum(awele.data[i,1:6])  
  awele.sum[i,2] = sum(awele.data[i,7:12])
}

# ajout des coups et du score
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

# On compare la relation entre le nombre de cases vides avec le coup jou? pour les 2 joueurs
chisq.test(selection[,1] , selection[,3])$p.value #0,08
chisq.test(selection[,2] , selection[,3])$p.value #0,07
chisq.test((awele.sum[,1]-awele.sum[,2]) , awele.gagne)$p.value #0,001

#--------------------------------------------------------
#-- Ajout du nombre de cases vides par joueur
#--------------------------------------------------------

# Cr?ation d'une nouvelle matrice 
awele.vides = data.frame(matrix(data=0, nr = nrow(awele.data), nc=4))

# ajout du nombre de case ? 0
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

# On compare la relation entre le nombre de cases vides avec le coup jou? pour les 2 joueurs
chisq.test(selection[,1] , selection[,3])$p.value #0,0022
chisq.test(selection[,2] , selection[,3])$p.value #0,0010

#--------------------------------------------------------
#-- Ajout du nombre de cases ? 1 ou 2 par joueur
#--------------------------------------------------------

# Cr?ation d'une nouvelle matrice 
awele.1ou2 = data.frame(matrix(data=0, nr = nrow(awele.data), nc=4 ))

# ajout du nombre de case ? 0
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

# Y a t'il un rapport entre le nombres de cases ? 1 et ? 2 et le coup jou? ?
chisq.test(awele.1ou2[,1] , awele.coup)$p.value #0,66
chisq.test(awele.1ou2[,2] , awele.coup)$p.value #0,25

# recuperation de tous les coups gagnant 
selection = awele.1ou2 [awele.gagne== "G", ]
plot(selection[,1:2], col=selection[,3])

# On compare la relation entre le nombre de cases ? 1ou2 avec le coup jou? pour les 2 joueurs
chisq.test(selection[,1] , selection[,3])$p.value #0,092
chisq.test(selection[,2] , selection[,3])$p.value #0,24


## Fonction qui rajoute les données au tableau existant
fx.completeData = function(data){
  # création d'une nouvelle frame avec 6 colonnes en plus
  decal = 6
  newData = data.frame(matrix(data=0, nr = nrow(data), nc=ncol(data)+ decal ))
  
  #ajout des colonnes de base
  for(i in 1:12){
    newData[,i] = data[,i];
    colnames(newData)[i] = colnames(data)[i]
  }
    
  
  #ajout des colonnes avec les sommes
  for(i in 1:nrow(data)){
    newData[i,13] = sum(data[i,1:6])
    newData[i,14] = sum(data[i,7:12])
  }
  colnames(newData)[13] = "sumJ"
  colnames(newData)[14] = "sumA"
  
  
  #ajout des colonnes avec les nombres vides
  for(i in 1:nrow(data)){
    newData[i,15] = sum(data[i,1:6]==0)
    newData[i,16] = sum(data[i,7:12]==0)
  }
  colnames(newData)[15] = "videsJ"
  colnames(newData)[16] = "videsA"
  
  #ajout des colonnes avec le nombre à 1 ou 2
  for(i in 1:nrow(data)){
    newData[i,17] = sum(data[i,1:6]==2) + sum(data[i,1:6]==1)
    newData[i,18] = sum(data[i,7:12]==2) + sum(data[i,7:12]==1)
  }
  colnames(newData)[17] = "1ou2J"
  colnames(newData)[18] = "1ou2A"
  
  
  #ajout des colonnes finales
  for(i in 19:20){
    newData[,i] = data[,i-decal];
    colnames(newData)[i] = colnames(data)[i-decal]
  }
  
  return (newData)
}

awele.dataBis = fx.completeData(awele.data)

#--------------------------------------------------------
# R?sultat
#--------------------------------------------------------

### TEST la somme des 3 premi?res cases ?
### --> test? : non
### TEST la somme des 3 derni?res cases ?
### --> test? : non

# en fonction de la somme ?
# en fonction du nombre de cases vides ?
# en foncion du nombre des case s? 1 ou ? 2 chez ton adversaire ?
           
           

#---------------------------------------------------------------
#-- Parties pas sur
#---------------------------------------------------------------


########
#Voir les donn?es
########
plot(awele.data[,1:12], col=awele.data[,14])

########
#Les attributs sont ils ind?pendants ? Utiliser le Test de Pearson
########
chisq.test(awele.data[,1], awele.data[,12])$p.value
cout("ca sert ? rien")

########
####Les attributs sont ils corr?l?s lin?airements ?
########
correlations = cor (awele.val)
#on el?ve la diagonale
diag (correlations) = 0
# Et on r?cup?re la valeur absolue
correlations = abs (correlations)
# On les affiches
print (correlations)


########
#### ACP ?
########
acp = PCA(awele.val, quanti.sup=awele.data[,14])
plot.PCA(acp)
cout("ca sert ? rien !")


########
#### NB 
#######
# On s?lectionne les instances qui correspondent aux coups jou?s par le vainqueur des affrontements
selection = awele.data [awele.data [, 14] == "G", ]
# Et on construit un mod?le de classification avec l'algorithme Naive bayes
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


