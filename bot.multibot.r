####################################
#   MultiBot
###################################


####################
### Initialisation
####################

# Chargement de la bibliothèque
library (e1071)
library("fdm2id") 
source("addData.r")
source ("bot.adlkiller.R")  #adlkiller
source ("bot.adl12.R")  #adlkiller
source ("bot.cart.R")  #adlkiller
source ("bot.svm2.R")  #adlkiller
source ("bot.nb.R")  #adlkiller



multibot = function (awele) {
  
 b1 = adlkiller(awele)
 b2 = adl12(awele)
 b3 = cart1(awele)
 b4 = svm2(awele)
 b5 = nb2(awele)
 
 solution = getCoupFromCoups(b1,b2,b3,b4,b5)
 return(solution)
}


getCoupFromCoups = function(b1, b2, b3, b4, b5){

  somme = coup = matrix(0, ncol=6, nrow=1)
  
  # calcul du résultat
  somme = as.matrix(b1 + b2 + b3 + b4 + b5)
  valMax = max(somme)
  
  # on compte combien de fois la valeur max apparait
  nbMax = sum(somme == valMax)
 
  if(nbMax > 1){
    b5null = sum(b5 == 0) == 6
    b4null = sum(b4 == 0) == 6
    
    # soit j'ai déjà plus que 3 bots, dans ce cas là je retourne b1
    if(b5null || b4null){
      coup = b1;
    }
    # soit j'ai encore 5 bots bots{
    else{
      vecNull = matrix(0, ncol=6, nrow=1)
      coup = getCoupFromCoups(b1,b2,b3,vecNull,vecNull)
    }
    
  }else{
    coup[somme == valMax] = 1
    coup[somme != valMax] = 0
    
  }
  
  return (as.data.frame(coup)) 
}