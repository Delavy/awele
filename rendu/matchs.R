###########################################################
# Prof

# Chargement du jeu d'Awélé
source ("awele.R")
source ("bot.nb.R")       #nb + nb2

###########################################################
# Nouveaux bots du rendu
source ("dpln.bot.adl.R")
source ("dpln.bot.adlsupp.R")
source ("dpln.bot.afd.R")
source ("dpln.bot.cart.R")
source ("dpln.bot.cartsupp.R")
source ("dpln.bot.knn.R")
source ("dpln.bot.mlp.R")
source ("dpln.bot.mlpsupp.R")
source ("dpln.bot.multibot.R")
source ("dpln.bot.nbsupp.R")
source ("dpln.bot.rl.R")
source ("dpln.bot.svm.R")
source ("dpln.bot.svmsupp.R")


# MATCH
awele.match (dpln.adlkiller, dpln.nbsupp)
