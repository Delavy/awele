#setwd ("/chemin/vers/les/fichiers/")

# Chargement du jeu d'Awélé en R
source ("awele.R")

# Chargement des bots nb et nb2
source ("bot.nb.R")

# Chargement du bot random
source ("bot.random.R")

# Matchs entre nb et random
print (awele.match (nb, random))

# Matchs entre nb2 et random
print (awele.match (nb2, random))

# Matchs entre nb et nb2
print (awele.match (nb, nb2))