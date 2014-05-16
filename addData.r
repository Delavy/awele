# Chargement de la bibliothèque
library (e1071)

setClass("dataSupp", representation(fonction = "function", decal = "numeric"))

#######################
## Fonctions disponibles : 
## - bidoua : fonction depuis un livre, qui calcule un potentiel pour chaque case
## - nbVidesAvantPleines : nombre de cases vides avant la première pleine
## - posMax : position de la case qui contient le plus de billes
## - somme : somme des billes
## - sum1ou2 : sommes des billes à 1ou2
## - unTour : nombre de cases qui peuvent faire plus d'un tour
## - vide : nombre de cases vides


######################
### Somme
######################
### somme$function : function qui ajoute la somme vides au données data à partir de la position posAdd
###   @param data les données. 
###   ---- Les 6 première lignes doivent contenir les données 
###   ---- posAdd:posAdd+decal doivent être vides car vont etre écrasées
###   @return data complété
### somme$decal : décalage apporté par l'ajout des données par la fonction
somme = new("dataSupp")
somme@fonction = function(data, posAdd){
  #ajout des colonnes avec les sommes
  for(i in 1:nrow(data)){
    data[i,posAdd] = sum(data[i,1:6])
    data[i,posAdd+1] = sum(data[i,7:12])
  }
  colnames(data)[posAdd] = "sumJ"
  colnames(data)[posAdd+1] = "sumA"  
  return (data)
}
# decalage
somme@decal = 2
########

######################
### Nombre de vides avant la première pleine
######################
### nbVidesAvantPleine$function : function qui ajoute la somme vides au données data à partir de la position posAdd
###   @param data les données. 
###   ---- Les 6 première lignes doivent contenir les données 
###   ---- posAdd:posAdd+decal doivent être vides car vont etre écrasées
###   @return data complété
### nbVidesAvantPleine$decal : décalage apporté par l'ajout des données par la fonction
nbVidesAvantPleine = new("dataSupp")
nbVidesAvantPleine@fonction = function(data, posAdd){
  #ajout des colonnes avec les sommes
  for(i in 1:nrow(data)){
    #chez A
    nbA = 0;
    for(j in 1:6){
      if(data[i,j]==0){
        nbA = nbA + 1
      }else{
        break;
      }
    }
    data[i,posAdd] = nbA
    
    # chez B
    nbB = 0
    for(j in 7:12){
      if(data[i,j]==0){
        nbB = nbB + 1
      }else{
        break;
      }
    }
    data[i,posAdd+1] = nbB
  }
  colnames(data)[posAdd] = "nbVidesBeforePleineJ"
  colnames(data)[posAdd+1] = "nbVidesBeforePleineA"  
  return (data)
}
# decalage
nbVidesAvantPleine@decal = 2
########

######################
### Pos Max
######################
### posMax$function : function qui ajoute la position de la maximum
###   @param data les données. 
###   ---- Les 6 première lignes doivent contenir les données 
###   ---- posAdd:posAdd+decal doivent être vides car vont etre écrasées
###   @return data complété
### posMAx$decal : décalage apporté par l'ajout des données par la fonction
posMax =  new("dataSupp")
posMax@fonction = function(data, posAdd){
  #ajout des colonnes avec les sommes
  for(i in 1:nrow(data)){
    data[i,posAdd] = match(max(data[i,1:6]), data[i,1:6])
    data[i,posAdd+1] = match(max(data[i,7:12]), data[i,7:12])
  }
  colnames(data)[posAdd] = "posMaxJ"
  colnames(data)[posAdd+1] = "posMaxA"  
  return (data)
}
# decalage
posMax@decal = 2
######################

######################
### Pos Max
######################
### posMax$function : function qui ajoute la position de la maximum
###   @param data les données. 
###   ---- Les 6 première lignes doivent contenir les données 
###   ---- posAdd:posAdd+decal doivent être vides car vont etre écrasées
###   @return data complété
### posMAx$decal : décalage apporté par l'ajout des données par la fonction
bidoua  =  new("dataSupp")
bidoua@fonction = function(data, posAdd){
  #ajout des colonnes avec les sommes
  for(i in 1:nrow(data)){
    accu1 = match(max(data[i,1:6]), data[i,1:6])
    for(j in 1:6){
      n = data[i,j]
      x = j - accu1
      data[i,posAdd+j-1] = (-1/2)*((n*n)+ (1-(2*x)*n - 2 ))
    }
    
    accu2 = match(max(data[i,7:12]), data[i,7:12])
    for(j in 7:12){
      n = data[i,j]
      x = j - accu2
      data[i,posAdd+j-1] = (-1/2)*((n*n)+ (1-(2*x)*n - 2 ))
    }        
  }
  
  for(n in posAdd:(posAdd+11)){
    colnames(data)[n] = paste ("bidou", n, sep = "")
  }

  return (data)
}
# decalage
bidoua@decal = 12
######################

######################
### Vide
######################
### vide$function : function qui ajoute la somme vides au données data à partir de la position posAdd
###   @param data les données. 
###   ---- Les 6 première lignes doivent contenir les données 
###   ---- posAdd:posAdd+decal doivent être vides car vont etre écrasées
###   @return data complété
### vide$decal : décalage apporté par l'ajout des données par la fonction
vide  =  new("dataSupp")
vide@fonction = function(data, posAdd){
  #ajout des colonnes avec les sommes
  for(i in 1:nrow(data)){
    data[i,posAdd] = sum(data[i,1:6]==0)
    data[i,posAdd+1] = sum(data[i,7:12]==0)
  }
  colnames(data)[posAdd] = "videsJ"
  colnames(data)[posAdd+1] = "videsA"  
  return (data)
}
# decalage
vide@decal = 2
######################

######################
### 1ou2
######################
### 1ou2$function : function qui ajoute la somme des case à 1 ou 2 au data à partir de la position posAdd
###   @param data les données. 
###   ---- Les 6 première lignes doivent contenir les données 
###   ---- posAdd:posAdd+decal doivent être vides car vont etre écrasées
###   @return data complété
### 1out2$decal : décalage apporté par l'ajout des données par la fonction
sum1ou2  =  new("dataSupp")
sum1ou2@fonction = function(data, posAdd){
  #ajout des colonnes avec les sommes
  for(i in 1:nrow(data)){
    data[i,posAdd]   = sum(data[i,1:6]==2) + sum(data[i,1:6]==1)
    data[i,posAdd+1] = sum(data[i,7:12]==2) + sum(data[i,7:12]==1)
  }
  colnames(data)[posAdd] = "unoudeuxJ"
  colnames(data)[posAdd+1] = "unoudeuxA"  
  return (data)
}
# decalage
sum1ou2@decal = 2
######################


######################
### 1tour
######################
### vide$function : function qui ajoute le nombre de cases qui peuvent faire 1 tour ou plus.
###   @param data les données. 
###   ---- Les 6 première lignes doivent contenir les données 
###   ---- posAdd:posAdd+decal doivent être vides car vont etre écrasées
###   @return data complété
### vide$decal : décalage apporté par l'ajout des données par la fonction
unTour  =  new("dataSupp")
unTour@fonction = function(data, posAdd){
  #ajout des colonnes avec les sommes
  for(i in 1:nrow(data)){
    sum1tourA = 0;
    sum1tourB = 0;
    
    for(j in 1:6){
      if(data[i,j]>17-(j-1)){
        sum1tourA = sum1tourA + 1
      }
    }
      
    for(j in 7:12){
      if(data[i,j]>17-(j-7)){
        sum1tourB = sum1tourB + 1
      }
    }    
  
    data[i,posAdd+1] = sum1tourB
    data[i,posAdd] = sum1tourA
    
    colnames(data)[posAdd] = "1tourJ"
    colnames(data)[posAdd+1] = "1tourA"  
  }
  return (data)
}
# decalage
unTour@decal = 2


########
### Fonction qui retourne la somme des décalage apportés par les fonctions dans listOfFx
### @param : listOfFx : liste des fonctions classes dataSupp
### @return : un int représentant le décalage total
addData.getDecalage = function(listOfFx){
  decal = 0;
  for(i in 1:length(listOfFx)){
    decal = decal + listOfFx[[i]]@decal
  }
  return(decal)
}

######################
### 1ou2
######################
### 1ou2$function : function qui ajoute la somme des case à 1 ou 2 au data à partir de la position posAdd
###   @param data les données. 
###   ---- Les 6 première lignes doivent contenir les données 
###   ---- posAdd:posAdd+decal doivent être vides car vont etre écrasées
###   @return data complété
### 1out2$decal : décalage apporté par l'ajout des données par la fonction
nulle  =  new("dataSupp")
nulle@fonction = function(data, posAdd){
  return (data)
}
# decalage
nulle@decal = 0
######################

########
### Function qui rajoute les données au tableau existant
### @param data : les données. 
### @param listOfFx : la liste des classes dataSupp
### @return : les données avec les colonnes rajoutées
addData.completeData = function(data, listOfFx){
  
  # récupération du décalage
  decal = addData.getDecalage(listOfFx)
  
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
  for(i in 1:length(listOfFx)){
    # on execute la fonction
    newData = listOfFx[[i]]@fonction(newData, pos)
    #on ajoute le décalage apporté par la fonction
    pos = pos + listOfFx[[i]]@decal
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
