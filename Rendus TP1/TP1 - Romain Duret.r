############################# TP 1 - Romain Duret - Polytech Marseille

####### EXERCICE 1 : 

my_data <- read.delim("~/Documents/Analyse\ Donnee/data1TP1.txt")
my_data2 <- read.delim("~/Documents/Analyse\ Donnee/data2TP1.txt")

head(my_data)
library(ggplot2)

par(mfrow = c(2,3))
plot(my_data$A, my_data$Y)
plot(my_data$B, my_data$Y)
plot(my_data$C, my_data$Y)
plot(my_data$D, my_data$Y)
plot(my_data$E, my_data$Y)

# Nous pouvons observer plusieurs courbes, présentant différentes corrélations entre les données et Y.
# Par exemple, A et B sont linéaires et monotones.
# D est linéaire et non-monotone, alors que E est non-linéaire et non-monotone.

####### EXERCICE 2 : 

# Implémentation de la fonction :
coeff_r <- function(X, Y) {
  result = cov(X, Y)/(sd(X)*sd(Y))
  result
}

# calcul pour chaque donnée et verification avec la fonction R cor()
coeff_r(my_data$A, my_data$Y)
cor(my_data$A, my_data$Y)
coeff_r(my_data$B, my_data$Y)
cor(my_data$B, my_data$Y)
coeff_r(my_data$C, my_data$Y)
cor(my_data$C, my_data$Y)
coeff_r(my_data$D, my_data$Y)
cor(my_data$D, my_data$Y)
coeff_r(my_data$E, my_data$Y)
cor(my_data$E, my_data$Y)

# Le coefficient R permet de savoir s'il y a une forte relation linéaire ou non
# entre deux variables.
# Plus la valeur obtenue entre deux variables - ou groupe de variables - est proche de
# 0, plus les deux variables sont indépendantes.
# Ainsi, la varibale avec la plus petite corrélation avec Y est donc la variable E.

####### EXERCICE 3 :  

# Implémentation de la fonction du coefficient de Spearman : 
coeff_spearman <- function(X, Y) {
  somme = 0
  N = length(X)
  R_x = rank(X)
  R_y = rank(Y)
  for (i in seq(1, N)) {
    provi = R_x[i] - R_y[i]
    a_add = provi^2
    somme = somme + a_add
  }
  
  result = 1 - ((6*somme)/(N^3 - N))
  result
}

# calcul pour chaque donnée et verification avec la fonction R cor()
coeff_spearman(my_data$A, my_data$Y)
cor(my_data$A, my_data$Y, method="spearman")
coeff_spearman(my_data$B, my_data$Y)
cor(my_data$B, my_data$Y, method="spearman")
coeff_spearman(my_data$C, my_data$Y)
cor(my_data$C, my_data$Y, method="spearman")
coeff_spearman(my_data$D, my_data$Y)
cor(my_data$D, my_data$Y, method="spearman")
coeff_spearman(my_data$E, my_data$Y)
cor(my_data$E, my_data$Y, method="spearman")

# Le coefficient de Spearman permet de savoir si une relation entre deux variables 
# est monotone ou non. -1 signifie une relation en opposition parfaite, 1 signifie un
# classement identique.
# Par rapport à la question 2, seulement les variables D et E présentent une différence 
# significative. D est bien strictement monotone, par exemple.

####### EXERCICE 4 :

# Une variable non-linéaire et non-monotone, comme entre E et Y, peut être tout
# simplement un polynome, c'est à dire une relation entre plusieurs variables.
# Plusieurs solutions peuvent être utilisées pour résoudre ce problème :
# faire une transformation, utiliser une fonction non-linéaire...

####### EXERCICE 5 : 

# Implémentation du test d'indépendance pour une variable quantitative : 
test_independance_var_quant = function(X, u) {
  moyenne_observee = mean(X)
  moyenne_theorique = u
  haut = abs(moyenne_observee - moyenne_theorique)
  bas = (sd(X)/sqrt(length(X)))
  resultat = haut/bas
  resultat
}

# pour 15 donnees et alpha 0.05, on a un niveau de signification t(n-1 = 14, 95%) = 2.145
if(test_independance_var_quant(my_data2$Marseille, 19)<=2.145) {
  print("H0 : hypothese validee, l'inflation n'a pas affecté le cout")
} else {
  print("H1 : hyothese invalide, l'inflation a affecté le cout ")
}

# Nous obtenons H1, ce qui signifie que l'inflation a effectué le coût de la vie.

####### EXERCICE 6 :

# Implémentation du test d'indépendance pour deux variables quantitatives : 
test_independance_deux_var_quant = function(X, Y) {
  m1 = mean(X)
  m2 = mean(Y)
  haut = abs(m1 - m2)
  marseille = sd(X)^2/length(X)
  aix = (sd(Y)^2)/length(Y)
  resultat = haut/sqrt(marseille+aix)
  print(resultat)
  resultat
}

print(test_independance_deux_var_quant(my_data2$Marseille, my_data2$Aix))

# pour n=15*2 donnees et alpha 0.05, on a un niveau de signification t(n-2 = 28, 95%) = 2.048
if(test_independance_deux_var_quant(my_data2$Marseille, my_data2$Aix)<=2.048) {
  print("H0 : hypothese validee, Aix et Marseille ont un niveau de vie semblable")
} else {
  print("H1 : hyothese invalide, il existe une dependance significatif entre Aix et Marseille ")
}

# Nous obtenons H1

# pour n=15*2 donnees et alpha 0.02, on a un niveau de signification t(n-2 = 28, 98%) = 2.468
if(test_independance_deux_var_quant(my_data2$Marseille, my_data2$Aix)<=2.468) {
  print("H0 : hypothese validee, Aix et Marseille ont un niveau de vie semblable")
} else {
  print("H1 : hyothese invalide, il existe une dependance significatif entre Aix et Marseille ")
}

# Nous obtenons H0

# Je ne sais pas interpreter ces résultats.

####### EXERCICE 7 : 

# b) Fonction du khi deux.
# Les variables correspondent aux données k, O, E de la formule :
fonction_khi_deux = function(nombre_classe_distinct, valeur_observee, valeur_theorique) {
  somme = 0
  for (i in seq(1, nombre_classe_distinct)) {
    Oi = valeur_observee[i]
    Ei = valeur_theorique[i]
    calcul = ((Oi - Ei)^2)/Ei
    somme = somme + calcul
  }
  somme
}

# valeurs théoriques : 859.5 / 19.875 / 21.9375 / 0.0625
nombre_phenotype = 4 # k
ratio_generiques = c(9, 3, 3, 1) # ratio génériques pour la présence de chaque phénotype
valeur_observee = c(1528, 106, 117, 381) # valeurs observees pour chaque phénotype de plante
nombre_plante_observee = sum(valeur_observee) # n, nombre de plante observee au total
valeur_theorique = c(0, 0, 0, 0)

# a)  calcul des valeurs theoriques : (nombre de plante * (ratio_du_phenotype/somme_des_ratios))
for(i in seq(1, nombre_phenotype)) {
  valeur_theorique[i] = nombre_plante_observee * (ratio_generiques[i]/sum(ratio_generiques))
}
print(fonction_khi_deux(nombre_phenotype, valeur_observee, valeur_theorique))
# On obtient : 966.61

# c) valeur théorique : 7.81
# différence vraiment grande = aucune corrélation

############ Exercice 8 : 

# donnees : 
form = rbind(c(29, 5, 46), c(40, 32, 8), c(18, 22, 0))
color = rbind(c(20, 60),  c(29, 51), c(12, 28))

n = sum(form) #somme du nombre de donéee.

# calcul des donnes theoriques avec une fonction :
matrice_theorique = function(valeur_observee) {
  theorique = matrix(nrow = nrow(valeur_observee), ncol = ncol(valeur_observee))
  for(i in seq(1, nrow(valeur_observee))) {
    for(j in seq(1, ncol(valeur_observee))) {
      theorique[i,j] = ( sum(valeur_observee[i,]) * sum(valeur_observee[,j]) ) / n
    }
  }
  theorique
}
val_theo_form = matrice_theorique(form)
val_theo_color = matrice_theorique(color)

# nouvelle fonction pour calculer khi deux avec une matrice : 
fonction_khi_deux_matrice = function(nombre_classe_distinct, nombre_different_param, valeur_observee, valeur_theorique) {
  # sum(((form - val_theo_form)^2)/val_theo_form)
  somme = 0
  for (i in seq(1, nombre_classe_distinct)) {
    for(j in seq(1, nombre_different_param)) {
      Oi = valeur_observee[i, j]
      Ei = valeur_theorique[i, j]
      calcul = ((Oi - Ei)^2)/Ei
      somme = somme + calcul
    }
  }
  somme
}
print(fonction_khi_deux_matrice(nrow(color), ncol(color), color, val_theo_color))
print(fonction_khi_deux_matrice(nrow(form), ncol(form), form, val_theo_form))
# alpha = 5%, 3 possibilités, donc, par lecture du tableau, on a valeur théorique = 5.99.

# la couleur : 2.39415, Cela vérifie H0 : les variables sont indépendantes, donc la variable n'est pas importante.
# En effet, le fait que les variables soient indépendantes font que la présence d'un mélanome n'affecte pas la couleur.
# la forme : 75,1564, Cela ne vérifie pas H0 : les variables sont dépendantes, donc la variable  est importante 
# pour détecter un mélanome, vu que la forme dépend de la présence - ou non - d'un mélanome.

########## Exercice 9 : 

#Les tests paramétriques se basent sur des distributions statistiques supposées dans les données. 
#Par conséquent, certaines conditions de validité doivent être vérifiées pour que le résultat d’un test paramétrique
#soit fiable. 

#D'après les questions précedentes, le test t de Student pour échantillons indépendants n’est fiable que si les
#données associées à chaque échantillon suivent une distribution normale et si les variances des échantillons sont homogènes.
#C'est pour cela qu'il est paramétrique.

#Les tests non-paramétriques ne se basent pas sur des distributions statistiques.
#Ils peuvent donc être utilisés même si les conditions de validité des tests paramétriques ne sont pas vérifiées.
#Il me semble qu'un test paramétrique est comme son nom l'indique basé sur l'estimation de paramètres des échantillons
#, or le test du Chi2 est une comparaison de proportions d'échantillons indépendants et des proportions (pourcentages...) 
#ne sont pas des paramètres à proprement parler
#C'est pour cela qu'il est non-paramétrique.

########## Exercice 10 :
