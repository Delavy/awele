#setwd ("/chemin/vers/les/fichiers/")

# Chargement du jeu d'Aw?l? en R
source ("awele.R")

# Chargement des bots nb et nb2
source ("bot.nb.R")

# Chargement du bot random
source ("bot.random.R")

source ("bot.pedro.R")
source ("bot.pedrobis.R")
source ("bot.adl.R")
# Matchs entre nb et random
print (awele.match (pedro, pedrobis))

# Matchs entre nb2 et random
print (awele.match (nb2, random))

# Matchs entre nb et nb2
print (awele.match (nb, nb2))

# Matchs entre botADL1 et nb
print (awele.match (botADL1, nb2))

# Matchs entre botADL2 et nb
print (awele.match (botADL2, nb))
