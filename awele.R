# Classe représentant une partie d'Awele en cours
setClass ("awele.class", representation (graines = "vector", joueurs = "vector", score = "vector"))

initialisation.awele.test = function (graines, joueur1 = "Joueur 1", joueur2 = "Joueur 2")
{
  r = list (graines = graines, joueurs = c (joueur1, joueur2), score = c (0, 0))
  class (r) = "awele.class"
  return (r)
}

# Initialisation de la partie d'Awele
initialisation.awele = function (joueur1 = "Joueur 1", joueur2 = "Joueur 2")
{
  r = list (graines = rep (4, 12), joueurs = c (joueur1, joueur2), score = c (0, 0))
  class (r) = "awele.class"
  return (r)
}

# Affichage de l'état du plateau de jeu
print.awele = function (x, ...)
{
  print (paste (x$joueurs [2], ":", score.adversaire (x)))
  print (paste (graines.adversaire (x) [6:1], collapse = " "))
  print (paste (graines.joueur (x), collapse = " "))
  print (paste (x$joueurs [1], ":", score.joueur (x)))
}

# État du plateau de jeu
graines = function (awele) awele$graines

# État du plateau de jeu (matrice de 12 colonnes)
graines.matrix = function (awele) matrix (awele$graines, ncol = 12)

# État du plateau de jeu du côté du joueur actif
graines.joueur = function (awele) awele$graines [1:6]

# État du plateau de jeu du côté de l'adversaire du joueur actif
graines.adversaire = function (awele) awele$graines [7:12]

# Nombre de graines encore en jeu
nb.graines = function (awele) sum (graines (awele))

# Nombre de graine du joueur actif
nb.graines.joueur = function (awele) sum (graines.joueur (awele))

# Nombre de graine de l'adversaire du joueur actif
nb.graines.adversaire = function (awele) sum (graines.adversaire (awele))

# score du joueur actif
score.joueur = function (awele) awele$score [1]

# score de l'adversaire du joueur actif
score.adversaire = function (awele) awele$score [2]

# Changement de joueur actif (au tour de l'adversaire)
change.joueur = function (awele)
{
  r = list (graines = c (graines.adversaire (awele), graines.joueur (awele)), joueurs = awele$joueurs  [c (2, 1)], score = awele$score  [c (2, 1)])
  class (r) = "awele.class"
  return (r)
}

# Teste la validité d'un coup en fonction de l'état du plateau de jeu
coups.valides = function (awele)
{
  valide = (graines.joueur (awele) > 0 & any (graines.adversaire (awele) > 0))
  valide = valide | ((1:6 + graines.joueur (awele)) > 6)
  return (valide)
}

# Choix d'un coup selon l'état du plateau de jeu (validité du coup) et la décision du joueur (score sur chacun des coups possibles)
choix.coup = function (awele, decision)
{
  scoring = decision
  scoring [!coups.valides (awele)] = -1
  return (which.max (scoring))
}

# Retourne la distance maximale parcourue par les graines quand elles sont distribués
distance = function (awele, choix)
{
  graines = graines (awele)
  d = graines [choix]
  d = d + (d %/% 12)
  if ((d %% 12) == 0) d = d + 1
  return (d)
}

# Retourne la dernière case de l'égrenage
dernier = function (awele, choix) return (((choix + distance (awele, choix) - 1) %% 12) + 1)

# Distribution des graines
egrenage = function (awele, choix)
{
  d = distance (awele, choix)
  repartition = tabulate (((choix - 1):(choix + d - 1) %% 12) + 1, 12)
  t = graines (awele)
  t = t + repartition
  t [choix] = 0
  r = list (graines = t, joueurs = awele$joueurs, score = awele$score)
  class (r) = "awele.class"
  return (r)
}

# Résolution des prises (en fonction de l'état du plateau après l'égrenage et de la dernière case remplie)
# Mise à jour du score
prises = function (awele, dernier)
{
  if (dernier > 6)
  {
    score = awele$score
    nb.adv = dernier - 6
    t.adv = graines.adversaire (awele) [(1:nb.adv)]
    valide = ((t.adv == 2) | (t.adv == 3))
    eval = matrix (F, nb.adv, nb.adv)
    eval [lower.tri (eval, T)] = T
    eval = sweep (eval, 1, valide, FUN = "&")
    prises = 6 + which ((colSums (eval) + 1:nb.adv) == (nb.adv + 1))
    t = graines (awele)
    s = sum (t [prises])
    t.ancien = t
    t [prises] = 0
    if (sum (t [6:12]) > 0) score [1] = score  [1] + s else t = t.ancien
    r = list (graines = t, joueurs = awele$joueurs, score = score)
    class (r) = "awele.class"
    awele = r
  }
  return (awele)
}

# Réalisation d'un coup en fonction de l'état du plateau de jeu et du choix qui a été fait par le joueur
jouer.coup = function (awele, choix)
{
  der = dernier (awele, choix)
  awele = egrenage (awele, choix)
  awele = prises (awele, der)
  return (awele)
}

# Retourne le résultat actuel (vainqueur) de la partie
resultat = function (awele)
{
  score = awele$score
  res = ""
  if (score [1] == score [2])
    res = paste ("Égalité (", score [1], " - ", score [1], ")", sep = "")
  else
  {
    smax = max (score)
    smin = min (score)
    res = paste (awele$joueurs [which.max (score)], " (", smax, " - ", smin, ")", sep = "")
  }
  return (res)
}

# Fonction intermédiaire pour l'affrontement de deux bots à l'Awélé
awele.exec = function (awele, bot1, bot2, debug = F)
{
  bots = c (bot1, bot2)
  while (nb.graines (awele) > 6 && score.adversaire (awele) < 25)
  {
    if (debug)
      print.awele (awele)
    if (all (coups.valides (awele) == F))
    {
      awele$score [1] = awele$score [1] + nb.graines.joueur (awele)
      awele$graines [1:12] = 0
    }
    else
    {
      decision = bots [[1]] (awele)
      if (length (decision) != 6)
        stop (paste ("Bot non conforme :", awele$joueurs [1]))
      print (paste (round (decision, 3), collapse = " "))
      bots = bots [c (2, 1)]
      choix = choix.coup (awele, decision)
      if (debug)
        print (paste ("Coup choisi :", choix))
      awele = jouer.coup (awele, choix)
      awele = change.joueur (awele)
      if (debug)
        print ("******************")
    }
  }
  if (debug)
    print.awele (awele)
  return (resultat (awele))
}

# Fonction principale
# Fait s'affronter deux bots à l'Awélé et retourne le résultat
awele.match = function (bot1, bot2, debug = F)
{
  awele = initialisation.awele (deparse (substitute (bot1)), deparse (substitute (bot2)))
  print (awele.exec (awele, bot1, bot2, debug))
  awele = initialisation.awele (deparse (substitute (bot2)), deparse (substitute (bot1)))
  print (awele.exec (awele, bot2, bot1, debug))
  return ("Terminé")
}