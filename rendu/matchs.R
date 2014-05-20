#setwd ("/chemin/vers/les/fichiers/")


###########################################################
# Prof

# Chargement du jeu d'Aw?l? en R
source ("awele.R")

source ("bot.nb.R")       #nb + nb2
source ("bot.random.R")   #random


###########################################################
# stufs

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



###########################################################
# Nouveaux bots du rendu
source ("rendu/dpln.bot.adl.R")
source ("rendu/dpln.bot.adlsupp.R")
source ("rendu/dpln.bot.afd.R")
source ("rendu/dpln.bot.cart.R")
source ("rendu/dpln.bot.cartsupp.R")
source ("rendu/dpln.bot.knn.R")
source ("rendu/dpln.bot.mlp.R")
source ("rendu/dpln.bot.mlpsupp.R")
source ("rendu/dpln.bot.multibot.R")
source ("rendu/dpln.bot.nbsupp.R")
source ("rendu/dpln.bot.rl.R")
source ("rendu/dpln.bot.svm.R")
source ("rendu/dpln.bot.svmsupp.R")


# MATCH
awele.match (dpln.adlkiller, dpln.adl12)



package.skeleton(name="dpln", path=".", code_files=c("rendu/dpln.bot.adl.R","rendu/dpln.bot.adlsupp.R","rendu/dpln.bot.afd.R","rendu/dpln.bot.cart.R","rendu/dpln.bot.cartsupp.R","rendu/dpln.bot.knn.R","rendu/dpln.bot.mlp.R","rendu/dpln.bot.mlpsupp.R","rendu/dpln.bot.multibot.R","rendu/dpln.bot.nbsupp.R","rendu/dpln.bot.rl.R","rendu/dpln.bot.svm.R","rendu/dpln.bot.svmsupp.R","rendu/dpln.addData.R"))
