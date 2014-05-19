####################################
#   MultiBot
###################################


####################
### Initialisation
####################

# Chargement de la bibliothèque
library (e1071)
library("fdm2id") 
source("dpln.addData.R")

source ("dpln.bot.adlsupp.R")  	# dpln.adl12, dpln.adlsum, dpln.adlkiller
source ("dpln.bot.cart.R")  	# dpln.cart
source ("dpln.bot.svmsupp.R")  	# dpln.svmsupp
source ("bot.nb.R")  			# dpln.nb

# Bot : chargement des 5 bots + retour du coup
dpln.multibot = function (awele) {
  
 b1 = dpln.adlkiller(awele)
 b2 = dpln.adl12(awele)
 b3 = dpln.cart(awele)
 b4 = dpln.svmsupp(awele)
 b5 = nb2(awele)
 
 solution = getCoupFromCoups(b1,b2,b3,b4,b5)
 return(solution)
}

#########################
# GetCoupFromCoups. retourne le coup le plus présent, 
# Si égalité, celui des 3 premier
# Si toujours égalité, celui du premier
# @param b1 ... b5 : coups proposé par les bots
# @return le coup le plus demandé
getCoupFromCoups = function(b1, b2, b3, b4, b5){

  somme = coup = matrix(0, ncol=6, nrow=1)
  
  # calcul du résultat
  somme = as.matrix(b1 + b2 + b3 + b4 + b5)
  valMax = max(somme)
  
  # on compte combien de fois la valeur max apparait
  nbMax = sum(somme == valMax)
 
  # si elle apparaît plusieurs fois
  if(nbMax > 1){
    b5null = sum(b5 == 0) == 6
    b4null = sum(b4 == 0) == 6
    
    # soit j'ai déjà plus que 3 bots, dans ce cas là je retourne b1
    if(b5null || b4null){
      coup = b1;
    }
    # soit j'ai encore 5 bots 
    else{
      vecNull = matrix(0, ncol=6, nrow=1)
      coup = getCoupFromCoups(b1,b2,b3,vecNull,vecNull)
    }    
  }
  ## sinon je retourne direct
  else{
    coup[somme == valMax] = 1
    coup[somme != valMax] = 0    
  }
  
  return (as.data.frame(coup)) 
}