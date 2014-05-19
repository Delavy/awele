#setwd ("/chemin/vers/les/fichiers/")

# Chargement du jeu d'Aw?l? en R
source ("awele.R")

source ("bot.nb.R")       #nb + nb2
source ("bot.random.R")   #random

source ("bot.pedro.R")    #pedro
source ("bot.pedrobis.R") #pedrobis

source ("dpln.bot.adlsupp.R")      #botADL1
source ("bot.adl.R")      #botADL1
source ("bot.adl12.R")     #adl12
source ("bot.adlsum.R")     #adlsum
source ("bot.adlkiller.R")  #adlkiller

source ("bot.afd.R")      #afd1
source ("bot.rl.R")       #RL
source ("bot.knn.R")      # knn1

source ("bot.ludo.R")     #mlpludo
source ("bot.mlpbis.R")   #mlpbis

source ("bot.cart.R")     # cart1 
source ("bot.cartbis.R")  # cartBis


##### Attention, prennent du temps à charger
source ("bot.svm2.R")   # svm2
source ("bot.svm.R")    # svm
#source ("bot.ludo.R")     #mlpludo


#AFD est pas concluant face à ADL1, RL est plus fort qu'AFD

# MATCH
awele.match (adl12, dpln.adlsupp)



