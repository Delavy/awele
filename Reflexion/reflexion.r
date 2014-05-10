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


########
#### Bootstrap
#######
fx.bootstrap = function(val, name){
  vseed=0;
  vnruns=100;
  cat("Lancement des calculs...","\n")
  resultat = c (0, 0, 0, 0)
  #bootstrap : 026 : ridicule 
  resultat[1] = bootstrap(method=naiveBayes,x=val,y=name, seed=vseed, nruns=vnruns)
  names(resultat)[1] = "NB"
  cat("NB  : ", resultat[1],"\n")
  
  #ALD 5
  resultat[2] = bootstrap(method=LDA,x=val, y=name, seed=vseed, nruns=vnruns)
  names(resultat)[2] = "ALD"
  cat("ALD : ", resultat[2],"\n")
  
  #AFD 
  resultat[3] =  bootstrap(method=CDA,x=val, y=name, seed=vseed, nruns=vnruns)
  names(resultat)[3] = "AFD"
  cat("AFD : ", resultat[3],"\n")
  
  #RL : 
  resultat[4] = bootstrap(method=LR,x=val, y=name, seed=vseed, nruns=vnruns)
  names(resultat)[4] = "RL"
  cat("RL : ", resultat[4],"\n")
  
  #tri des resultats  
  resultat = sort(resultat);
  return(resultat)
  
}


######### Preparation des objets d'enrichissement de données
### Initialisation de la liste contenant les objets d'enrichisement
pedro.fx = list()
pedro.fx[[1]] = somme
#pedro.fx[[2]] = somme
#pedro.fx[[3]] = vide
#pedro.fx[[4]] = nbVidesAvantPleine
#pedro.fx[[5]] = posMax


### Calcul du décalage apporté par la liste
pedro.calculeDecalage = function(){
  decal = 0
  for(i in 1:length(pedro.fx)){
    decal = decal + pedro.fx[[i]]@decal
  }
  return (decal)
}



########
### Function qui rajoute les données au tableau existant
### @param data : les données. 
### @return : les données avec les colonnes rajoutées
fx.completeData = function(data){
  
  # récupération du décalage
  decal = pedro.calculeDecalage()
  
  # création d'une nouvelle frame avec les colonnes supplémentaires
  newData = data.frame(matrix(data=0, nr = nrow(data), nc=ncol(data)+ decal ))
  
  # Recopie des colonnes de base, ainsi que des nom si il y en as
  asColNames = length(colnames(data))>0;  
  for(i in 1:12){
    newData[,i] = data[,i];
    if(asColNames){
      colnames(newData)[i] = colnames(data)[i]
    }
  }
  
  # Ajout des données par execution des fonctions
  pos = 13
  # execution des fonctions 1 par 1
  for(i in 1:length(pedro.fx)){
    # on execute la fonction
    newData = pedro.fx[[i]]@fonction(newData, pos)
    #on ajoute le décalage apporté par la fonction
    pos = pos + pedro.fx[[i]]@decal
  }
  
  
  # si il reste des colonnes
  if(ncol(newData)>(12+decal)){    
    #ajout des colonnes finales
    for(i in (13+decal):ncol(newData)){
      newData[,i] = data[,i-decal];
      colnames(newData)[i] = colnames(data)[i-decal]
    }
  }
  
  return (newData)
}
###################################################
## Lancement des calculs

# On essaye avec les données normales
data = read.table ("awele.data", sep = ",", header = T)
res = fx.bootstrap(data[,1:12], data[,13])
barplot(res)

tot = cbind(as.data.frame(res))
names(tot) = "normal"

######### Sum1ou2
# génération des données
pedro.fx[[1]] = sum1ou2
decal = pedro.calculeDecalage()
dataPlus = fx.completeData(data)

# calcul de bootstart
res = fx.bootstrap(dataPlus[,1:12+decal], dataPlus[,14+decal])
barplot(res)

# ajout au tableau
tot = cbind(tot, as.data.frame(res))
colnames(tot) = c("normal","sum1ou2")



######### vides
# génération des données
pedro.fx[[1]] = vide
decal = pedro.calculeDecalage()
dataPlus = fx.completeData(data)

# calcul de bootstart
res = fx.bootstrap(dataPlus[,1:12+decal], dataPlus[,14+decal])
barplot(res)

# ajout au tableau
noms = colnames(tot)
tot = cbind(tot, as.data.frame(res))
colnames(tot) = c(noms,"vide")



######### somme ça marche pas

######### nb Vides avant pleine
# génération des données
pedro.fx[[1]] = nbVidesAvantPleine
decal = pedro.calculeDecalage()
dataPlus = fx.completeData(data)

# calcul de bootstart
res = fx.bootstrap(dataPlus[,1:12+decal], dataPlus[,14+decal])
barplot(res)

# ajout au tableau
noms = colnames(tot)
tot = cbind(tot, as.data.frame(res))
colnames(tot) = c(noms,"nbVidesAvantPleines")


######### posMax
# génération des données
pedro.fx[[1]] = posMax
decal = pedro.calculeDecalage()
dataPlus = fx.completeData(data)

# calcul de bootstart
res = fx.bootstrap(dataPlus[,1:12+decal], dataPlus[,14+decal])
barplot(res)

# ajout au tableau
noms = colnames(tot)
tot = cbind(tot, as.data.frame(res))
colnames(tot) = c(noms,"posMax")



######### sum1ou2, vide
# génération des données
pedro.fx[[1]] = sum1ou2
pedro.fx[[2]] = vide
decal = pedro.calculeDecalage()
dataPlus = fx.completeData(data)

# calcul de bootstart
res = fx.bootstrap(dataPlus[,1:12+decal], dataPlus[,14+decal])
barplot(res)

# ajout au tableau
noms = colnames(tot)
tot = cbind(tot, as.data.frame(res))
colnames(tot) = c(noms,"1ou2+vides")


######### sum1ou2, vide, nbVides, posMax
# génération des données
pedro.fx[[1]] = sum1ou2
pedro.fx[[2]] = vide
pedro.fx[[3]] = nbVidesAvantPleine
pedro.fx[[4]] = posMax
decal = pedro.calculeDecalage()
dataPlus = fx.completeData(data)

# calcul de bootstart
res = fx.bootstrap(dataPlus[,1:12+decal], dataPlus[,14+decal])
barplot(res)

# ajout au tableau
noms = colnames(tot)
tot = cbind(tot, as.data.frame(res))
colnames(tot) = c(noms,"1ou2+vides+nbVides+posmax")

