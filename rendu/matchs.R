###########################################################
# Prof

# Chargement du jeu d'Awélé
source ("awele.R")
source ("bot.nb.R")       #nb + nb2
source ("bot.random.R")   #random

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
