# TP 2 d'Analyse de Donnée - Romain Duret
library(scatterplot3d) # import des bibliothèques 
library("plot3D")
#library("plot3Drgl")
library(psych)

A <- read.delim("~/Documents/DataAnalysis/data1TP2.txt")

###### EXERCICE 1 : Tracez en dimension 3 le nuage de 10 points

nuage3d <- scatterplot3d(A)

###### EXERCICE 2 : Ecrivez le tableau centré B et la matrice de covariance V

B = scale(A, center=TRUE, scale=FALSE)
# Scale is generic function whose default method centers and/or scales the columns of a numeric matrix.
# scale = false car on ne veut pas scale les valeurs
V = cov(A)

###### EXERCICE 3 : Déterminez le représentation spectrale (valeurs propres et valeurs propres de V)

x = eigen(V)
val_propre_V = x$values # valeurs propres
vec = x$vectors # vecteur propres
print(vec)

###### EXERCICE 4 : Indiquez les axes principaux dans l'ordre : 
# Dans notre cas, les axes principaux sont bien ordonnées.
# En effet, la stature est le plus grand et se trouve en première position,
# Ensuite vient le poids puis la taille, la plus petite valeur.

###### EXERCICE 5 : Générez le tableau C en multipliant B par les vecteurs propres de V.

C = B %*% vec
print(C)
princomp(A)$scores # on vérifie les résultats

##### EXERCICE 6 : Observez en dimension 3 le nuage de points du premier axe principal

scatterplot3d(C)
print(c(0,-100*vec[1,1]))
scatter3D(x=c(0,-300*vec[1,1]), y=c(0,-300*vec[2,1]), z=c(0,-300*vec[3,1]), add=TRUE, type='l')


xi<- c(1:length(C[,1]))
print(C[,1])

##### EXERCICE 7 : Représentez le nuage de points en dimension 2, projetés des points de départ sur 
# le plan formé des deux premiers axes principaux
  
projection = matrix(c(C[,1], C[,2]), ncol=2)

plot(projection[,1], projection[,2], main="Nuage de Point",
     xlab="Stature", ylab ="Poids", pch=19)

##### EXERCICE 8 : Interprétez les résultats obtenus


