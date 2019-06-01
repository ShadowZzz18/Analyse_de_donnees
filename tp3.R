#BRUNA Arnaud
#PUAUX Tristan

#Question_1

# Definition du nombre de lignes m et de colonnes col
m = 300
col = 2

# Creation d'une matrice de m lignes et col colones remplie de 0
mat <- matrix(data = 0, m , col)

# Remplacement des valeurs des 100 premieres lignes par des variables x et y uniformes sur [0,1]
mat[1:100, 1:2] <- runif(200, min = 0, max = 1)

# Remplacement des valeurs des 200 lignes suivantes par des variables x et y gaussiennes independantes 
# en respectant les valeurs des moyennes et variances
mat[101:200,1] <- rnorm(100, 4 , 1)
mat[101:200,2] <- rnorm(100 , 0 , 1)
mat[201:300,1] <- rnorm(100, 0.5 , sqrt(2))
mat[201:300,2] <- rnorm(100, 6 , sqrt(2))

# Creation d'une matrice servant de vecteur couleur
color <- matrix(0, nrow=300, ncol=1)
color[1:100] <- 'blue'
color[101:200] <- 'green'
color[201:300] <- 'red'

# Affichage des valeurs de mat avec 3 couleurs differentes pour chaque 100 valeurs
plot(mat, col=color)

#Question_2
# On doit faire une matrice de distance des points 2 a 2 (matrice carree) pour regrouper les points entre eux et obtenir des classes de points, ici 3 classes

mat2<-mat
id <- diag(300)
# On calcule les distances entre les points (classes)
C<-dist(mat, method = "euclidean", diag = FALSE, upper = FALSE)
tab_dist<-as.matrix(C)

# On arrete la classification lorsqu'on a 3 classes
while(nrow(tab_dist)>3){
  # On trouve les points qui ont la distance minimale
  min_dist<-which(tab_dist==min(C),arr.ind = T)
  min1=min_dist[1,1]
  min2=min_dist[1,2]
  #On calcule les coordonnees du barycentre de la nouvelle classe
  mat[min1,1]<-(mat[min1,1]+mat[min2,1])/2
  mat[min1,2]<-(mat[min1,2]+mat[min2,2])/2
  # On met à jour la matrice des distances pour tenir compte de cette classe et supprimer les precedentes
  id[min1,]<-id[min1,]+id[min2,]
  id<-id[-min2,]
  mat<-mat[-min2,]
  # On recalcule les distances avec la nouvelle classe
  C<-dist(mat, method = "euclidean", diag = FALSE, upper = FALSE)
  tab_dist<-as.matrix(C)
}


#Question_3

# Cette fonction sert à definir des couleurs pour rendre visibles les 3 classes de points
id<-t(id)
z<-c(1:300)
for (i in 1:nrow(id)){
  if(id[i,1]==1){
    z[i]=1
  }
  if(id[i,2]==1){
    z[i]=2
  }
  if(id[i,3]==1){
    z[i]=3
  }
  
}

# On realise ici l'affichage des classes
plot(mat2,  col = c("red", "blue", "green")[z])
points(mat, pch=19)

