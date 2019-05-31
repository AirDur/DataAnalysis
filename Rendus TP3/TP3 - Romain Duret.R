# TP 3 d'Analyse de Donnée - Romain Duret & Thomas Gattaz

###### EXERCICE 1 : Générer des points du plan (m points), sous la forme d'un tableau ou d'une matrice
###### (matrice à m lignes et 2 colonnes). Les classes pourront être representés par des valeurs binaires
###### Les distances entre classes seront simplement les distances euclidiennes entre les centres des classes.
##### 1 : Génération de nuages de points (m = 300 points, 3 modes de génération)

nombre_point_par_generation = 100
m = nombre_point_par_generation*3

### a : génération de x et y uniformes sur [0;1]

A_x = runif(nombre_point_par_generation, min = 0, max = 1)
A_y = runif(nombre_point_par_generation, min = 0, max = 1)
matrice_A <- cbind(A_x, A_y)
plot(A_x, A_y, col = "red")

### b : génération de x et y gaussiennes (indépendantes) de variance 1 (moyenne de x : 4, y centrée)

# mean = moyenne de la variable. 0 = variable centrée
# sd = écart type. De variance 1 donc on doit mettre la racine carrée de la variance.
B_x = rnorm(nombre_point_par_generation, mean = 4, sd = sqrt(1)) 
B_y = rnorm(nombre_point_par_generation, mean = 0, sd = sqrt(1))
matrice_B <- cbind(B_x, B_y)
plot(B_x, B_y, col = "blue")

### c : génération de x et y gaussiennes (indépendantes) de variance 2 (moyenne de x : 0,5, y de moyenne : 6)

C_x = rnorm(nombre_point_par_generation, mean = 0.5, sd = sqrt(2)) 
C_y = rnorm(nombre_point_par_generation, mean = 6, sd = sqrt(2))
matrice_C <- cbind(C_x, C_y)
plot(C_x, C_y, col = "green")

# Union des 3 groupes en une valeur : 
valeurs <- rbind(matrice_A, matrice_B, matrice_C)
rownames(valeurs)<-c(1:300) #nommage
colors <- matrix(1, nrow = nombre_point_par_generation, ncol = 1)
colors2 <- matrix(2, nrow = nombre_point_par_generation, ncol = 1)
colors3 <- matrix(3, nrow = nombre_point_par_generation, ncol = 1)
Z <- rbind(colors,colors2,colors3)

### On visualisera l'ensemble des points obtenus :
plot(valeurs, col = c("red", "blue", "green")[Z])

###### EXERCICE 2 : Fonction de classification ascendante hiérarchique, où K (nombre de classe final) spécifiée en entrée
###### Sortie : matrice binaire (m lignes, K colonnes) représentant la classification.

###### Gestion des classes faite par matrice carrée C d'ordre ùm, initialement matrice identité, pour laquelle
###### la fusion des classes(i, j) conduit à faire la somme des lignes, remplaçant la ligne i, la ligne j étant mise à 0
###### On calculera pour la nouvelle classe la distance aux autres classes, toutes les distances étant sous forme d'un
###### tableau, qui sera donc également actualisé dans la boucle.

###### Les calculs de moyennes par classe sont calculés pour chaque nouvelle classe. A chaque classe, on associe
###### sa moyenne et son nombre de point. POur l'agrégation des classes, on choisit les classes la distance minimale.

# Fonction de classification ascendante hiérarchique
# Entrée valeurs : tout les points
# Entrée K : nombre de classe finale 
# Entrée m : nombre de points
# Sortie : matrice_binaire de taille (m lignes, K colonnes) représentant la classication
fonction_classification_ascendante_hierarchque = function(valeurs, K, m){
  
  # matrice distance contenant les distance entre tout les points de la matrice
  dist <- as.matrix(dist(valeurs,method="euclidean"))

  #  on enleve les valeurs inutiles : 
  dist[upper.tri(dist)] <- NA 
  diag(dist) <- NA

  # tant que nombre de ligne de dist > nombre de classes
  while(nrow(dist) > K){
    # recherce de la distance minimum :
    
    elim <- which(dist==min(dist,na.rm=T),arr.ind = T)
    
    # et on recupère la position : 
    elim_X <- elim[,1]
    elim_Y <- elim[,2]
    # calcul moyenne :
    moyenne_X <- (valeurs[elim_X,1] + valeurs[elim_Y,1]) / 2
    moyenne_Y <- (valeurs[elim_X,2] + valeurs[elim_Y,2]) / 2
    
    # remplace ligne par moyenne des deux lignes :
    valeurs[elim_X,1] <- moyenne_X
    valeurs[elim_X,2] <- moyenne_Y
    
    # renommage des lignes :
    rownames(valeurs)[elim_X]<-paste(rownames(valeurs)[elim_X],rownames(valeurs)[elim_Y])
    
    # suppression ligne fusionné :
    valeurs <- valeurs[-elim_Y,]
    dist <- dist[-elim_Y,]
    dist <- dist[,-elim_Y] 
    
    # on recaclul la matrice distance : 
    dist <- as.matrix(dist(valeurs,method="euclidean"))
    dist[upper.tri(dist)] <- NA
    diag(dist) <- NA
  }
  
  # retourne la matrice avec les centres des classes 
  return (valeurs)
}

###### EXERCICE 3 : Représentation du nuage de point, coloré en fonction des index des classes.

# Utilisation du code : 
C <- fonction_classification_ascendante_hierarchque(valeurs, 3, m)
print(C)

# Récupération des points dans chaque classe : 
tableau <- rownames(C)
classe1 <- as.numeric(unlist(strsplit(tableau[1]," ")))
classe2 <- as.numeric(unlist(strsplit(tableau[2]," ")))
classe3 <- as.numeric(unlist(strsplit(tableau[3]," ")))

# Regroupement des points avec les classes associées :
classe <- cbind(valeurs, c(0))
for(i in 1:length(classe1)){
  classe[classe1[i],3] <- 1
}
for(i in 1:length(classe2)){
  classe[classe2[i],3] <- 2
}
for(i in 1:length(classe3)){
  classe[classe3[i],3] <- 3
}
C <- cbind(C, c(1,2,3))

# affichage nuages de points avec les centres de classes :
plot(classe, pch=4, col=factor(classe[,3]))
points(C, pch=19, col="purple", bg=factor(C[,3]), cex=2)

######  Vérification avec le programme donné :
D <- dist(matrice, method = "euclidean")
AscHierarchique <- hclust(D, method= "complete")
plot(AscHierarchique, cex = 0.6, hang = -1)
cluster = cutree(AscHierarchique,3)

###### le centre de chaque classe, avec l'intertie relative intra-classe : 
inertie <- sort(AscHierarchique$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
points(c(2, 3, 4), inertie[c(2, 3, 4)], col = c("green", "red", 
                                                 "blue"), cex = 2.5, lwd = 4)

# coloriage du dendrogramme avec les classes choisis :
plot(AscHierarchique, labels = FALSE, main = "Partition en 2, 3 et 4 classes", 
     axes = FALSE, hang = -1)
rect.hclust(AscHierarchique, 2, border = "green")
rect.hclust(AscHierarchique, 3, border = "red")
rect.hclust(AscHierarchique, 4, border = "blue")

# fonction de calcul des barycentres
barycentre2 <- function(dfxy, fac, wt = rep(1, length(fac))){
  f1 <- function(cl) {
    n <- length(cl)
    cl <- as.factor(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1:n) + n * (unclass(cl) - 1)] <- 1
    dimnames(x) <- list(names(cl), levels(cl))
    data.frame(x)
  }
  dfdistri <- f1(fac) * wt
  dfdistri <- t(t(dfdistri)/unlist(lapply(dfdistri, sum)))
  coo2 <- t(dfdistri) %*% as.matrix(data.frame(dfxy))
  rownames(coo2) <- levels(fac)
  coo2
}

ba1 <- barycentre2(valeurs, cluster)
print(ba1)

# Distance entre barycentre du nuage de point et le barycentre des classes
# le tout multiplié par le nombre de valeurs : 
db1 <- colSums((t(ba1) - colMeans(valeurs))^2) * table(cluster)

print(db1)
# l'inertie inter (% de l'inertie totale) : 
internie_inter <- sum(db1)/sum((t(valeurs)-colMeans(valeurs))^2)
print(internie_inter)

# l'inertie intra (1 - internie inter) :
internie_intra <- 1 - internie_inter
print(internie_intra)

