# Chargement de la bibliothèque
library (e1071)


######################
### Somme
######################
### somme$function : function qui ajoute la somme vides au données data à partir de la position posAdd
###   @param data les données. 
###   ---- Les 6 première lignes doivent contenir les données 
###   ---- posAdd et posAdd+1 doivent être vides car vont etre écrasées
###   @return data complété
### somme$decal : décalage apporté par l'ajout des données par la fonction
somme = list(fonction = NULL, decal = NULL)
somme$fonction = function(data, posAdd){
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
somme$decal = 2
########

######################
### Nombre de vides avant la première pleine
######################
### nbVidesAvantPleine$function : function qui ajoute la somme vides au données data à partir de la position posAdd
###   @param data les données. 
###   ---- Les 6 première lignes doivent contenir les données 
###   ---- posAdd et posAdd+1 doivent être vides car vont etre écrasées
###   @return data complété
### nbVidesAvantPleine$decal : décalage apporté par l'ajout des données par la fonction
nbVidesAvantPleine = list(fonction = NULL, decal = NULL)
nbVidesAvantPleine$fonction = function(data, posAdd){
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
nbVidesAvantPleine$decal = 2
########

######################
### Pos Max
######################
### posMax$function : function qui ajoute la position de la maximum
###   @param data les données. 
###   ---- Les 6 première lignes doivent contenir les données 
###   ---- posAdd et posAdd+1 doivent être vides car vont etre écrasées
###   @return data complété
### posMAx$decal : décalage apporté par l'ajout des données par la fonction
posMax  = list(fonction = NULL, decal = NULL)
posMax$fonction = function(data, posAdd){
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
posMax$decal = 2
######################

######################
### Vide
######################
### vide$function : function qui ajoute la somme vides au données data à partir de la position posAdd
###   @param data les données. 
###   ---- Les 6 première lignes doivent contenir les données 
###   ---- posAdd et posAdd+1 doivent être vides car vont etre écrasées
###   @return data complété
### vide$decal : décalage apporté par l'ajout des données par la fonction
vide  = list(fonction = NULL, decal = NULL)
vide$fonction = function(data, posAdd){
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
vide$decal = 2
######################

######################
### 1ou2
######################
### 1ou2$function : function qui ajoute la somme des case à 1 ou 2 au data à partir de la position posAdd
###   @param data les données. 
###   ---- Les 6 première lignes doivent contenir les données 
###   ---- posAdd et posAdd+1 doivent être vides car vont etre écrasées
###   @return data complété
### 1out2$decal : décalage apporté par l'ajout des données par la fonction
sum1ou2  = list(fonction = NULL, decal = NULL)
sum1ou2$fonction = function(data, posAdd){
  #ajout des colonnes avec les sommes
  for(i in 1:nrow(data)){
    data[i,posAdd]   = sum(data[i,1:6]==2) + sum(data[i,1:6]==1)
    data[i,posAdd+1] = sum(data[i,7:12]==2) + sum(data[i,7:12]==1)
  }
  colnames(data)[posAdd] = "1ou2J"
  colnames(data)[posAdd+1] = "1ou2A"  
  return (data)
}
# decalage
sum1ou2$decal = 2
######################


########
### Function qui ajoute la somme au données data à partir de la position posAdd
### @param : data les données. 
### ---- Les 6 première lignes doivent contenir les données 
### ---- posAdd et posAdd+1 doivent être vides car vont etre écrasées
### @posAdd : la position à laquelle les données vont être ajoutées
### @fx : l'objet qui contient fx$fonction et fx$decal
addData = function(data, posAdd, fx){
  
  #decal = fx$decal
  
  # création de la nouvelle matrice en ajoutant le nombre de colonne de décal
  #newData = data.frame(matrix(data=0, nr = nrow(data), nc=ncol(data)+decal ))
  
  # ajout des colonnes de base
#  for(i in 1:(posAdd-1)){
#    newData[,i] = data[,i];
#    colnames(newData)[i] = colnames(data)[i]
#  }
  
  # ajout des colonne de fx
  newData = fx$fonction(data,posAdd)
  
  # Si l'insertion ne se faisait pas à la fin, je rajoute les colonnes après l'insertion
#  if(ncol(data)>=posAdd){    
#    for(i in posAdd:ncol(data)){            
#      newData[,i+decal] = data[,i]
#      colnames(newData)[i+decal] = colnames(data)[i]
#    }
#  }
  
  return (newData)
}