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


coeff_r <- function(X, Y) {
  result = cov(X, Y)/(sd(X)*sd(Y))
  result
}

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

coeff_spearman <- function(X, Y) {
  somme = 0
  N = length(X)
  R_x = rank(X)$
    R_y = rank(Y)
  for (i in seq(1, N)) {
    provi = R_x[i] - R_y[i]
    a_add = provi^2
    somme = somme + a_add
  }
  
  result = 1 - ((6*somme)/(N^3 - N))
  result
}

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

test_independance_var_quant = function(X, u) {
  moyenne_observee = mean(X)
  moyenne_theorique = u
  haut = abs(moyenne_observee - moyenne_theorique)
  bas = (sd(X)/sqrt(length(X)))
  resultat = haut/bas
  resultat
}

if(test_independance_var_quant(my_data2$Marseille, 19)<=2.145) {
  # pour 15 donnees et alpha 0.05, on a un niveau de signification t(n-1 = 14, 95%) = 2.145
  print("H0 : hypothese validee, l'inflation n'a pas affecté le cout")
} else {
  print("H1 : hyothese invalide, l'inflation a affecté le cout ")
}

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

if(test_independance_deux_var_quant(my_data2$Marseille, my_data2$Aix)<=2.048) {
  # pour 15*2 donnees et alpha 0.05, on a un niveau de signification t(n-2 = 28, 95%) = 2.048
  print("H0 : hypothese validee, Aix et Marseille ont un niveau de vie semblable")
} else {
  print("H1 : hyothese invalide, il existe une dependance significatif entre Aix et Marseille ")
}

if(test_independance_deux_var_quant(my_data2$Marseille, my_data2$Aix)<=2.468) {
  # pour 15*2 donnees et alpha 0.02, on a un niveau de signification t(n-2 = 28, 95%) =2.468
  print("H0 : hypothese validee, Aix et Marseille ont un niveau de vie semblable")
} else {
  print("H1 : hyothese invalide, il existe une dependance significatif entre Aix et Marseille ")
}

####### EXERCICE 7 : 

# b) Fonction du khi deux.
# k, O, E
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
# 
print(fonction_khi_deux(nombre_phenotype, valeur_observee, valeur_theorique))
# 966.61

# c) valeur théorique : 7.81
# différence vraiment grande = aucune corrélation

# Exercice 8 : 
