# Chargement de la biblioth?que
library (e1071)
# Chargement des donn?es
awele.data = read.table ("awele.data", sep = ",", header = T)

####################
#   BotGRL (glmnet)# BOT NON FONCTIONNEL
####################

###VERSION 1
# Fonction de construction du modèle
glmnet1.create.model = function (dataset)
{
  selection = awele.data [dataset [, 14] == "G", ]
  model = glmnet(selection [, 1:12], selection [, 13], family="multinomial")
  return (model)
}

glmnet1.model = glmnet1.create.model (awele.data)
