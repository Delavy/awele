#setwd ("/chemin/vers/les/fichiers/")

# Chargement du jeu d'Aw?l? en R
source ("awele.R")

# Chargement des bots nb et nb2
source ("bot.nb.R")

# Chargement du bot random
source ("bot.random.R")
source ("bot.svm.R")
source ("bot.pedro.R")
source ("bot.pedrobis.R")
source ("bot.adl.R") 
source ("bot.rl.R")
source ("bot.afd.R")
#AFD est pas concluant face à ADL1, RL est plus fort qu'AFD

# Matchs entre nb et afd
print (awele.match (nb, svm1))

# Matchs entre nb et random
print (awele.match (pedro, nb))

# Matchs entre nb2 et random
print (awele.match (nb2, random))

# Matchs entre nb et nb2
print (awele.match (nb, nb2))

# Matchs entre botADL1 et nb
print (awele.match (botADL1, nb2))

# Matchs entre botADL2 et nb
print (awele.match (botADL2, nb))
